module GCC.DataTypes
( DataValue(..)
, ControlValue(..)
, EnvFrames(..)
, GCC(..)
, DebugState(..)
, Stack
, stackPop
, stackPopMulti
, stackPush
, initGCC
, allocEnvFrame
, setEnvFrameVal
, getEnvFrameValRelative
) where

import qualified Data.Map as Map

-- Abstract Stack data type --
data Stack a =
    EmptyStack
  | Push a (Stack a)
    deriving (Show)

stackPop :: Stack a -> (a, Stack a)
stackPop EmptyStack = error "pop from empty stack"
stackPop (Push x s) = (x, s)

stackPopMulti :: Stack a -> Int -> ([a], Stack a)
stackPopMulti stack n
    | n == 0    = ([], stack)
    | otherwise = (val:vals, stack'')
        where (val,  stack')  = stackPop stack
              (vals, stack'') = stackPopMulti stack' $ n - 1

stackPush :: Stack a -> a -> Stack a
stackPush stack val = Push val stack

-- GCC data types --
-- Data stack values
data DataValue =
    DataInt Int
  | DataPair DataValue DataValue
  | DataClosure Int Int -- code pointer, env pointer
    deriving (Show)

-- Control stack values
data ControlValue =
    ControlStop
  | ControlJoin Int       -- code pointer
  | ControlReturn Int Int -- code pointer, env pointer
    deriving (Show)

-- Environment frame type
data EnvFrame = EnvFrame
    { envValues   :: Map.Map Int DataValue
    , frameSize   :: Int
    , parentFrame :: Int
    } deriving (Show)

-- Environment frame storage interface
data EnvFrames = EnvFrames
    { envTable :: Map.Map Int EnvFrame
    , nextKey  :: Int
    } deriving (Show)

allocEnvFrame :: EnvFrames -> Int -> Int -> (Int, EnvFrames)
allocEnvFrame (EnvFrames tbl nxt) size parent = (nxt, EnvFrames (Map.insert nxt (EnvFrame Map.empty size parent) tbl) newNxt)
    where newNxt = nxt + 1

setEnvFrameVal :: EnvFrames -> Int -> Int -> DataValue -> EnvFrames
setEnvFrameVal frames@(EnvFrames tbl nxt) fid vid val = EnvFrames tbl' nxt
    where (EnvFrame v s p)  = getEnvFrame frames fid
          frame'
              | vid < s   = EnvFrame (Map.insert vid val v) s p
              | otherwise = error ("setting value in illegal index: " ++ show fid ++ " " ++ show vid) 
          tbl'   = Map.insert fid frame' tbl

getEnvFrame :: EnvFrames -> Int -> EnvFrame
getEnvFrame (EnvFrames tbl nxt) fid = frame
    where
        frame
            | Map.member fid tbl = tbl Map.! fid
            | otherwise          = error ("accessing illegal frame: " ++ show fid)

getEnvFrameVal :: EnvFrames -> Int -> Int -> DataValue
getEnvFrameVal frames@(EnvFrames tbl nxt) fid vid = frameVal
    where
        (EnvFrame v s p)  = getEnvFrame frames fid
        frameVal
            | vid < s   = v Map.! vid
            | otherwise = error ("getting value fron illegal index: " ++ show fid ++ " " ++ show vid) 

getEnvFrameValRelative :: EnvFrames -> Int -> Int -> Int -> DataValue
getEnvFrameValRelative frames fid frel vid
    | frel == 0 = getEnvFrameVal frames fid vid
    | otherwise = getEnvFrameValRelative frames parid (frel - 1) vid
        where (EnvFrame v s parid)  = getEnvFrame frames fid

-- Debug state
data DebugState =
    NoDebug
  | DebugBreak
  | DebugPrint DataValue
    deriving (Show)

-- GCC state
data GCC = GCC
    { codePointer  :: Int                -- what instruction to execute next
    , dataStack    :: Stack DataValue    -- Data Stack to store intermediate values and return results from functions
    , controlStack :: Stack ControlValue -- Control Stack for things like return address etc
    , envFrames    :: EnvFrames          -- Environment Frames storage
    , envFrameRef  :: Int                -- Current Environment Frame id
    , debugState   :: DebugState         -- Value to print in debug mode (see DBUG instruction)
    } deriving (Show)

initGCC :: GCC
initGCC = GCC 0 EmptyStack (stackPush EmptyStack ControlStop) (EnvFrames Map.empty 0) (-1) NoDebug
