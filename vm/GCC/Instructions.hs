module GCC.Instructions
( Instruction(..)
, instructionExec
, isInstructionLine
, parseInstruction
) where

import Data.List
import qualified Data.Char as Char

import GCC.DataTypes

-- Instruction constructors
-- Derived read can be used for parsing from normalized code lines
-- e.g. (read "LD 0 0") :: Instruction == LD 0 0
data Instruction
    = STOP
    | LDC Int | LD Int Int
    | ADD | SUB | MUL | DIV
    | CEQ | CGT | CGTE | ATOM
    | CONS | CAR | CDR
    | SEL Int Int | JOIN
    | LDF Int | AP Int | RTN
    | DUM Int | RAP Int -- not implemented yet
      -- Debug extensions
    | BRK | DBUG
      -- Tail-call extensions
    | TSEL Int Int | TAP Int | TRAP Int
     -- Procedural extensions
    | ST Int Int
    deriving (Show, Read)

-- Parsing helpers
lineBeforeComment :: String -> String
lineBeforeComment line = takeWhile (/=';') line 

isInstructionLine :: String -> Bool
isInstructionLine line = not $ all Char.isSpace $ lineBeforeComment line

parseInstruction :: String -> Instruction
parseInstruction line = read normalizedLine
  where
    symbols        = words $ lineBeforeComment line
    normalizedLine = intercalate " " symbols

-- Instructions impl
instructionExec :: GCC -> Instruction -> Maybe GCC
instructionExec (GCC cp ds cs ef efr _) instruction = case instruction of
    STOP     -> Nothing -- no next state, machine terminated
    LDC x    -> Just $ GCC cpInc newDs cs ef efr NoDebug
      where
        newDs = stackPush ds $ DataInt x 
    LD  n i  -> Just $ GCC cpInc newDs cs ef efr NoDebug
      where
        fid      = getParentEnvFrameId ef efr n
        frame    = assertEnvFrameNotDummy $ getEnvFrame ef fid
        frameVal = getEnvFrameVal frame i
        newDs    = stackPush ds frameVal
    ADD      -> intBinaryOp  (+)
    SUB      -> intBinaryOp  (-)
    MUL      -> intBinaryOp  (*)
    DIV      -> intBinaryOp  div
    CEQ      -> intBinaryCmp (==)
    CGT      -> intBinaryCmp (>)
    CGTE     -> intBinaryCmp (>=)
    ATOM     -> Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        (val, ds') = stackPop ds
        result     = case val of
            (DataInt _) -> 1
            _           -> 0
        nextDs     = stackPush ds' $ DataInt result
    CONS     -> Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        ([r,l], ds') = stackPopMulti ds 2
        pair         = DataPair l r
        nextDs       = stackPush ds' pair
    CDR      -> Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        (val, ds') = stackPop ds
        nextDs     = case val of
            (DataPair _ r) -> stackPush ds' r
            _              -> error "CDR: data type mismatch"
    CAR      -> Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        (val, ds') = stackPop ds
        nextDs     = case val of
            (DataPair l _) -> stackPush ds' l
            _              -> error "CAR: data type mismatch"
    SEL t f  -> Just $ GCC nextCp nextDs nextCs ef efr NoDebug
      where
        (val, nextDs) = stackPop ds
        nextCs        = stackPush cs (ControlJoin cpInc)
        nextCp        = case val of
            (DataInt x) -> if (x == 0) then f else t
            _           -> error "SEL: data type mismatch"
    JOIN     -> Just $ GCC nextCp ds nextCs ef efr NoDebug
      where
        (nextCp, nextCs) = case stackPop cs of
            (ControlJoin x, cs') -> (x, cs')
            _                    -> error "JOIN: control mismatch"
    LDF x    -> Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        closure = DataClosure x efr
        nextDs  = stackPush ds closure
    AP  n    -> Just $ GCC nextCp nextDs nextCs nextEf nextEfr NoDebug
      where
        ((nextCp, closureEfr), ds') = case stackPop ds of
            ((DataClosure nc e), s) -> ((nc, e), s)
            _                       -> error "AP: data type mismatch"
        (nextEfr, ef')   = allocEnvFrame ef n closureEfr
        (nextDs, nextEf) = stackToEnvCopy ds' ef' nextEfr n
        nextCs           = stackPush cs $ ControlReturn cpInc efr
    RTN      -> case stackPop cs of
        -- TODO: if the current frame is not captured by LDF
        -- we can dealocate it right on RTN instead of waiting for GC
        (ControlStop, _)              -> Nothing -- no next state, machine terminated 
        (ControlReturn cp' efp', cs') -> Just $ GCC cp' ds cs' ef efp' NoDebug
        _                             -> error "RTN: control mismatch"
    DUM _    -> Nothing -- TODO
    RAP _    -> Nothing -- TODO
    BRK      -> Just $ GCC cpInc ds cs ef efr DebugBreak
    DBUG     -> Just $ GCC cpInc nextDs cs ef efr (DebugPrint val)
      where
        (val, nextDs) = stackPop ds
    TSEL t f -> Just $ GCC nextCp nextDs cs ef efr NoDebug
      where
        (val, nextDs) = stackPop ds
        nextCp        = case val of
            (DataInt x) -> if (x == 0) then f else t
            _           -> error "TSEL: data type mismatch"
    TAP  n   -> Just $ GCC nextCp nextDs cs nextEf nextEfr NoDebug
      where
        -- TODO: tail-call optimization:
        -- Do not allocate frame if we can reuse the current one
        -- Check: frame is big enough AND frame is not captured by LDF
        ((nextCp, closureEfr), ds') = case stackPop ds of
            ((DataClosure nc e), s) -> ((nc, e), s)
            _                       -> error "TAP: data type mismatch"
        (nextEfr, ef')   = allocEnvFrame ef n closureEfr
        (nextDs, nextEf) = stackToEnvCopy ds' ef' nextEfr n
    TRAP _   -> Nothing -- TODO
    ST n i   -> Just $ GCC cpInc newDs cs newEf efr NoDebug
      where
        (val, newDs) = stackPop ds
        fid      = getParentEnvFrameId ef efr n
        frame    = assertEnvFrameNotDummy $ getEnvFrame ef fid
        frame'   = modEnvFrameVal frame i val
        newEf    = setEnvFrame ef fid frame'
  where
    cpInc          = cp + 1
    intBinaryCmp f = intBinaryOp (\x y -> if (x `f` y) then 1 else 0)
    intBinaryOp  f = Just $ GCC cpInc nextDs cs ef efr NoDebug
      where
        ([r,l], ds') = stackPopMulti ds 2
        nextDs       = case (l, r) of
            (DataInt x, DataInt y) -> stackPush ds' (DataInt $ x `f` y)
            _                      -> error "data type mismatch: Int required"

-- Private helper functions
stackToEnvCopy :: Stack DataValue -> EnvFrames -> Int -> Int -> (Stack DataValue, EnvFrames)
stackToEnvCopy stack ef fr n
    | n == 0    = (stack, ef)
    | otherwise = stackToEnvCopy stack' ef' fr (n - 1)
  where
    (val, stack') = stackPop stack
    ef' = setEnvFrameVal ef fr (n - 1) val
