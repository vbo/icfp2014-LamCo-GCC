module GCC.Runner
( step
) where

import qualified Data.Map as Map

import GCC.Instructions
import GCC.DataTypes

step :: Map.Map Int Instruction -> GCC -> (Instruction, Maybe GCC)
step instructions gcc = (instruction, instructionExec gcc instruction)
    where
        cp = codePointer gcc
        instruction
            | Map.member cp instructions = instructions Map.! cp
            | otherwise                  = error ("bad code pointer: " ++ show cp)
