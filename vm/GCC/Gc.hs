module GCC.Gc (garbageCollect) where

-- Simple mark-and-sweep style garbage collector.
-- Algorithm:
-- 1. Collect all top-level referenced frames from:
--    - closures on data stack 
--    - frame refs on control stack
--    - current env ref
-- 2. Mark referenced env frames recursively (recurse to parent frames and closure frame vals)
-- 3. Go through all env frames, removing marks and deleting non-marked frames

import qualified Data.Map as Map
import GCC.DataTypes

garbageCollect :: GCC -> GCC
garbageCollect gcc = setEnvFrames gcc . deleteUnreachable . markAsReachable topLevelRefs . resetReachable $ envFrames gcc
    where
        topLevelRefs     = curFrame : (dataStackRefs ++ controlStackRefs)
        curFrame         = envFrameRef gcc
        dataStackRefs    = stackFold collectClosureRefs [] $ dataStack gcc
        controlStackRefs = stackFold collectReturnRefs []  $ controlStack gcc

collectReturnRefs :: ControlValue -> [Int] -> [Int]
collectReturnRefs v acc = case v of
    ControlReturn _ e -> e:acc
    _                 -> acc

collectClosureRefs ::  DataValue -> [Int] -> [Int]
collectClosureRefs v acc = case v of
    DataClosure _ e -> e:acc
    _               -> acc

markAsReachable :: [Int] -> EnvFrames -> EnvFrames
markAsReachable refs origEf = case refs of
    []     -> origEf
    (x:xs) ->
        if x == -1 -- TODO: make it hackless
            then markAsReachable xs origEf
            else markAsReachable xs (markOneFrameReachable x origEf)

markOneFrameReachable :: Int -> EnvFrames -> EnvFrames
markOneFrameReachable ref ef
    | notMarked = markAsReachable referenced . setEnvFrame ef ref $ frame { frameReachable = True }
    | otherwise = ef
    where
        frame       = getEnvFrame ef ref
        notMarked   = not . frameReachable $ frame
        referenced  = (parentFrame frame) : inFrameRefs (envValues frame)
        inFrameRefs = Map.fold collectClosureRefs []

deleteUnreachable :: EnvFrames -> EnvFrames
deleteUnreachable (EnvFrames tbl n) = EnvFrames (Map.filter frameReachable tbl) n

resetReachable :: EnvFrames -> EnvFrames
resetReachable (EnvFrames tbl n) = EnvFrames tbl' n
    where tbl' = Map.map (\f -> f { frameReachable = False }) tbl
