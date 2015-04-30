module GCC.DataTypes
( DataValue(..)
, ControlValue(..)
, EnvFrames(..)
, EnvFrame(..)
, GCC(..)
, setEnvFrames
, DebugState(..)
, Stack
, stackPop
, stackPopMulti
, stackPush
, stackFold
, initGCC
, allocEnvFrame
, getParentEnvFrameId
, getEnvFrameVal
, setEnvFrameVal
, modEnvFrameVal
, getEnvFrame
, setEnvFrame
, assertEnvFrameNotDummy
) where

import qualified Data.Map as Map

-- Abstract Stack data type --
data Stack a
    = EmptyStack
    | Push a (Stack a)
    deriving (Show)

stackPop :: Stack a -> (a, Stack a)
stackPop EmptyStack = error "pop from empty stack"
stackPop (Push x s) = (x, s)

stackPopMulti :: Stack a -> Int -> ([a], Stack a)
stackPopMulti stack n
    | n == 0    = ([], stack)
    | otherwise = (val:vals, stack'')
  where
    (val,  stack')  = stackPop stack
    (vals, stack'') = stackPopMulti stack' $ n - 1

stackPush :: Stack a -> a -> Stack a
stackPush stack val = Push val stack

stackFold :: (b -> a -> a) -> a -> Stack b -> a
stackFold f z xs = case xs of
    EmptyStack    -> z
    Push x tailxs -> f x (stackFold f z tailxs)

-- GCC data types --
-- Data stack values
data DataValue
    = DataInt Int
    | DataPair DataValue DataValue
    | DataClosure Int Int -- code pointer, env pointer
    deriving (Show)

-- Control stack values
data ControlValue
    = ControlStop
    | ControlJoin Int       -- code pointer
    | ControlReturn Int Int -- code pointer, env pointer
    deriving (Show)

-- Environment frame type
data EnvFrame = EnvFrame
    { envValues      :: !(Map.Map Int DataValue)
    , frameSize      :: !Int
    , parentFrame    :: !Int
    , frameReachable :: !Bool
    , isDummy        :: !Bool
    } deriving (Show)

-- Environment frame storage interface
data EnvFrames = EnvFrames
    { envTable :: !(Map.Map Int EnvFrame)
    , nextKey  :: !Int
    } deriving (Show)

allocEnvFrame :: EnvFrames -> Int -> Int -> (Int, EnvFrames)
allocEnvFrame (EnvFrames tbl nxt) size parent = (nxt, EnvFrames tbl' nxt')
  where
    tbl' = Map.insert nxt (EnvFrame Map.empty size parent True False) tbl
    nxt' = nxt + 1

getEnvFrame :: EnvFrames -> Int -> EnvFrame
getEnvFrame (EnvFrames tbl _) fid = frame
  where
    frame
        | Map.member fid tbl = tbl Map.! fid
        | otherwise          = error ("accessing illegal frame: " ++ show fid)

setEnvFrame :: EnvFrames -> Int -> EnvFrame -> EnvFrames
setEnvFrame frames fid fr = frames { envTable = Map.insert fid fr (envTable frames) }

getParentEnvFrameId :: EnvFrames -> Int -> Int -> Int
getParentEnvFrameId frames fid skip
    | skip == 0 = fid
    | otherwise = getParentEnvFrameId frames (parentFrame frame) (skip - 1)
  where
    frame = getEnvFrame frames fid

modEnvFrameVal :: EnvFrame -> Int -> DataValue -> EnvFrame
modEnvFrameVal (EnvFrame v s p _ d) vid val
    | vid < s = EnvFrame (Map.insert vid val v) s p True d
    | otherwise = error ("setting value in illegal index: " ++ show vid) 

setEnvFrameVal :: EnvFrames -> Int -> Int -> DataValue -> EnvFrames
setEnvFrameVal frames fid vid val = setEnvFrame frames fid $ modEnvFrameVal (getEnvFrame frames fid) vid val

assertEnvFrameNotDummy :: EnvFrame -> EnvFrame
assertEnvFrameNotDummy frame
    | isDummy frame = error ("non-dummy frame expected")
    | otherwise     = frame

getEnvFrameVal :: EnvFrame -> Int -> DataValue
getEnvFrameVal frame vid = frameVal
  where
    frameVal
        | vid < (frameSize frame) = (envValues frame) Map.! vid
        | otherwise               = error ("getting value fron illegal index: " ++ show vid) 

-- Debug state
data DebugState
    = NoDebug
    | DebugBreak
    | DebugPrint DataValue
    deriving (Show)

-- GCC state
data GCC = GCC
    { codePointer  :: !Int                  -- what instruction to execute next
    , dataStack    :: !(Stack DataValue)    -- Data Stack to store intermediate values and return results from functions
    , controlStack :: !(Stack ControlValue) -- Control Stack for things like return address etc
    , envFrames    :: !EnvFrames            -- Environment Frames storage
    , envFrameRef  :: !Int                  -- Current Environment Frame id
    , debugState   :: !DebugState           -- Value to print in debug mode (see DBUG instruction)
    } deriving (Show)

setEnvFrames :: GCC -> EnvFrames -> GCC
setEnvFrames gcc ef = gcc { envFrames = ef }

initGCC :: GCC
initGCC = GCC 0 EmptyStack (stackPush EmptyStack ControlStop) (EnvFrames Map.empty 0) (-1) NoDebug
