import System.Environment
import qualified Data.Map as Map
import qualified System.IO as SIO

import Debug
import GCC.Instructions(Instruction, isInstructionLine, parseInstruction)
import GCC.DataTypes(GCC, initGCC)
import GCC.Runner(step)

main :: IO ()
main = do
    instructions <- loadInstructions
    inputLoop instructions initGCC

loadInstructions :: IO (Map.Map Int Instruction)
loadInstructions = do
    args <- getArgs
    let inputFile = (args !! 0)
    input <- SIO.openFile inputFile SIO.ReadMode
    lines <- hGetLines input
    SIO.hClose input
    let instructions = Map.fromList . addLineNumbers . map parseInstruction . filter isInstructionLine $ lines
    return instructions

inputLoop :: Map.Map Int Instruction -> GCC -> IO ()
inputLoop instructions gcc = do
    cmd <- getLine
    -- ignore actual cmd - assume step
    let (instruction, gcc') = step instructions gcc
    print instruction
    proceed gcc'
    where
        proceed Nothing    = return ()
        proceed (Just gcc) = do
            print gcc
            inputLoop instructions gcc

hGetLines :: SIO.Handle -> IO [String]
hGetLines h = do
    ineof <- SIO.hIsEOF h
    if ineof
        then return []
    else do
        line <- SIO.hGetLine h
        more <- hGetLines h
        return $ line:more

addLineNumbers :: [Instruction] -> [(Int, Instruction)]
addLineNumbers = zip [0..]

