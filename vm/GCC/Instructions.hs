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
data Instruction =
    STOP
  | LDC Int | LD Int Int
  | ADD | SUB | MUL | DIV
  | CEQ | CGT | CGTE | ATOM
  | CONS | CAR | CDR
  | SEL Int Int | JOIN
  | LDF Int | AP Int | RTN
  | DUM Int | RAP Int -- not implemented yet
    deriving (Show, Read)

-- Parsing helpers
lineBeforeComment :: String -> String
lineBeforeComment line = takeWhile (/=';') line 

isInstructionLine :: String -> Bool
isInstructionLine line = not $ all Char.isSpace $ lineBeforeComment line

parseInstruction :: String -> Instruction
parseInstruction line = read normalizedLine
    where symbols        = words $ lineBeforeComment line
          normalizedLine = intercalate " " symbols

-- Instructions action impl
instructionExec :: GCC -> Instruction -> Maybe GCC
instructionExec (GCC cp ds cs ef efr) instruction = case instruction of
    STOP    -> Nothing -- no next state, machine terminated
    LDC x   -> Just $ GCC cpInc (stackPush ds $ DataInt x) cs ef efr
    LD  n i -> Just $ GCC cpInc (stackPush ds $ getEnvFrameValRelative ef efr n i) cs ef efr
    ADD     -> intBinaryOp  (+)
    SUB     -> intBinaryOp  (-)
    MUL     -> intBinaryOp  (*)
    DIV     -> intBinaryOp  div
    CEQ     -> intBinaryCmp (==)
    CGT     -> intBinaryCmp (>)
    CGTE    -> intBinaryCmp (>=)
    CONS    -> Just $ GCC cpInc nextDs cs ef efr
        where
            ([r,l], ds') = stackPopMulti ds 2
            pair         = (DataPair l r)
            nextDs       = stackPush ds' pair
    CDR     -> Just $ GCC cpInc nextDs cs ef efr
        where
            (val, ds') = stackPop ds
            nextDs     = case val of
                (DataPair _ r) -> stackPush ds' r
                otherwise      -> error "CDR: data type mismatch"
    CAR     -> Just $ GCC cpInc nextDs cs ef efr
        where
            (val, ds') = stackPop ds
            nextDs     = case val of
                (DataPair l _) -> stackPush ds' l
                otherwise      -> error "CAR: data type mismatch"
    SEL t f -> Just $ GCC nextCp nextDs nextCs ef efr
        where
            (val, nextDs) = stackPop ds
            nextCs        = stackPush cs (ControlJoin cpInc)
            nextCp        = case val of
                (DataInt x) -> if (x == 0) then f else t
                otherwise   -> error "SEL: data type mismatch"
    JOIN    -> Just $ GCC nextCp ds nextCs ef efr
        where
            (nextCp, nextCs) = case stackPop cs of
                (ControlJoin x, cs') -> (x, cs')
                otherwise   -> error "JOIN: control mismatch"
    LDF x   -> Just $ GCC cpInc nextDs cs ef efr
        where
            closure = DataClosure x efr
            nextDs  = stackPush ds closure
    AP  n   -> Just $ GCC nextCp nextDs nextCs nextEf nextEfr
        where
            ((DataClosure nextCp closureEfr), ds') = case stackPop ds of
                (c@(DataClosure _ _), s) -> (c, s)
                otherwise                -> error "AP: data type mismatch"
            (nextEfr, ef')   = allocEnvFrame ef n closureEfr
            (nextDs, nextEf) = stackToEnvCopy ds' ef' nextEfr n
            nextCs           = stackPush cs $ ControlReturn cpInc efr
    RTN     -> case stackPop cs of
        (ControlStop, _)              -> Nothing -- no next state, machine terminated 
        (ControlReturn cp' efp', cs') -> Just $ GCC cp' ds cs' ef efp'
        otherwise                     -> error "RTN: control mismatch"
    DUM n   -> Nothing -- TODO
    RAP n   -> Nothing -- TODO
    where
        cpInc          = cp + 1
        intBinaryCmp f = intBinaryOp (\x y -> if (x `f` y) then 1 else 0)
        intBinaryOp  f = Just $ GCC cpInc nextDs cs ef efr
            where
                ([r,l], ds') = stackPopMulti ds 2
                nextDs       = case (l, r) of
                    (DataInt x, DataInt y) -> stackPush ds' (DataInt $ x `f` y)
                    otherwise              -> error "data type mismatch: Int required"

-- Private helper functions
stackToEnvCopy :: Stack DataValue -> EnvFrames -> Int -> Int -> (Stack DataValue, EnvFrames)
stackToEnvCopy stack ef fr n
    | n == 0    = (stack, ef)
    | otherwise = stackToEnvCopy stack' ef' fr (n - 1)
        where
            (val, stack') = stackPop stack
            ef' = setEnvFrameVal ef fr (n - 1) val
