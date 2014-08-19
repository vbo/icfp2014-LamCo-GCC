module GCC.Runner
( step
) where

import qualified Data.Map as Map

import GCC.Instructions(Instruction, instructionExec)
import GCC.DataTypes(GCC, codePointer)
import GCC.Gc(garbageCollect)

step :: Map.Map Int Instruction -> GCC -> (Instruction, Maybe GCC)
step instructions gcc = (instruction, fmap garbageCollect $ instructionExec gcc instruction)
    where
        cp = codePointer gcc
        instruction
            | Map.member cp instructions = instructions Map.! cp
            | otherwise                  = error ("bad code pointer: " ++ show cp)
