import System.Environment
import qualified Data.Map as Map
import qualified System.IO as SIO

import Text.Show.Pretty(ppShow)

import Debug
import GCC.Instructions(Instruction, isInstructionLine, parseInstruction)
import GCC.DataTypes(GCC, codePointer, initGCC, debugState, DebugState(..))
import GCC.Runner(step)

main :: IO ()
main = do
    instructions <- loadInstructions
    let gcc = initGCC
    putStrLn $ ppShow gcc
    inputLoop instructions gcc

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
    putStrLn ""
    cmd <- getLine
    case cmd of
        "st" -> do stepDo gcc
        "co" -> do contLoop gcc
        ""   -> do loop gcc
        otherwise -> do
            putStrLn "! Unrecognized command. Use: st, co"
            loop gcc
    where
        loop         = inputLoop instructions
        stepDo gcc   = do
            let (instruction, gcc') = step instructions gcc
            putStrLn $ "! Executed: " ++ show instruction
            case gcc' of
                Nothing    -> return()
                (Just gcc) -> case debugState gcc of
                    DebugPrint x -> do putStrLn ("! Debug: " ++ show x); next
                    otherwise    -> next
                    where next = do dump gcc; loop gcc
        contLoop gcc = do
            let (instruction, gcc') = step instructions gcc
            case gcc' of
                Nothing    -> do dump gcc; return()
                (Just gcc) -> case debugState gcc of
                    DebugPrint x -> do putStrLn ("! Debug: " ++ show x); contLoop gcc
                    DebugBreak   -> do
                        putStrLn ("! Break at:" ++ (show $ codePointer gcc))
                        dump gcc
                        loop gcc
                    NoDebug      -> contLoop gcc
        dump gcc = putStrLn $ ppShow gcc

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

