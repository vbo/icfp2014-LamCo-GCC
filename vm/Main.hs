import System.Environment
import qualified Data.Map as Map
import qualified System.IO as SIO

import Text.Show.Pretty(ppShow)

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
    code_lines <- hGetLines input
    SIO.hClose input
    let instructions = Map.fromList . addLineNumbers . map parseInstruction . filter isInstructionLine $ code_lines
    return instructions

inputLoop :: Map.Map Int Instruction -> GCC -> IO ()
inputLoop instructions inGcc = do
    putStrLn ""
    cmd <- getLine
    case cmd of
        "st" -> do stepDo inGcc
        "co" -> do contLoop inGcc
        ""   -> do loop inGcc
        _    -> do
            putStrLn "! Unrecognized command. Use: st, co"
            loop inGcc
  where
    dump         = putStrLn . ppShow
    loop         = inputLoop instructions
    stepDo gcc   = do
        let (instruction, gcc') = step instructions gcc
        putStrLn $ "! Executed: " ++ show instruction
        case gcc' of
            Nothing       -> return()
            (Just newGcc) -> case debugState newGcc of
                DebugPrint x -> do putStrLn ("! Debug: " ++ show x); next
                _            -> next
              where
                next = do dump newGcc; loop newGcc
    contLoop gcc  = do
        let (_, gcc') = step instructions gcc
        case gcc' of
            Nothing       -> do dump gcc; return()
            (Just newGcc) -> case debugState newGcc of
                DebugPrint x -> do putStrLn ("! Debug: " ++ show x); contLoop newGcc
                DebugBreak   -> do
                    putStrLn ("! Break at:" ++ (show $ codePointer newGcc))
                    dump newGcc
                    loop newGcc
                NoDebug      -> contLoop newGcc

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

