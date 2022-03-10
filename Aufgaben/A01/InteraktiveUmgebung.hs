{-|
Module      : InteraktiveUmgebung
Description : An interactive environment for the prolog interpreter
Maintainer  : Jowan Sulaiman and Kjell Rothenburger
The module contains the following functions $'start', $'interactiveLoop',
$'processInput', $'parseGoal', $'validGoal', $'readFileSave', $'helpHeader,
'$'header' and $'requestSolution'.
The description of each function can be found below.
-}
module InteraktiveUmgebung
 where

import Type
import Substitutionen
import SLD_Resolution
import PrettyPrinting
import Parser

-- Starts the interactive program
start :: IO ()
start = do
 putStr header
 interactiveLoop Nothing bfs Nothing

-- Loop that reads the users input until the command ":q" terminates the program
interactiveLoop ::  Maybe Prog -> Strategy -> Maybe String -> IO ()
interactiveLoop p strat file = do
  putStr "?- "
  s <- getLine
  processInput s p strat file

-- Processes the input String
processInput :: String -> Maybe Prog -> Strategy -> Maybe String -> IO ()
-- input string; loaded program; current strategy; current fileName
processInput s p strat file
 | s == ":h"                 = putStrLn helpHeader >> (interactiveLoop p strat file)
 | s == ":p"                 =  case p of
                                 Nothing     -> putStrLn "No program loaded." >> (interactiveLoop p strat file)
                                 (Just prog) -> putStrLn "Loaded program:" >> putStrLn (pretty prog) >> (interactiveLoop p strat file)
 | s == ":s dfs"             = putStrLn "Strategy set to depth-first search." >> interactiveLoop p dfs file
 | s == ":s bfs"             = putStrLn "Strategy set to breadth-first search." >> interactiveLoop p bfs file
 | s == ":q"                 = putStrLn "Successful termination."
 | validGoal s               = case p of
                                 Nothing     -> putStrLn "No program loaded." >> (interactiveLoop p strat file)
                                 (Just prog) -> case (solveWith prog (parseGoal s) strat) of
                                                  []     -> putStrLn "No solution." >> interactiveLoop p strat file
                                                  (x:xs) -> putStr (show x) >> requestSolution p strat file xs
 | s == ":r"                 = case file of
                                Nothing -> putStrLn "There is no last loaded file." >> interactiveLoop p strat file
                                Just f  -> readFileSave p strat f
 | take 3 s == ":l "         = readFileSave p strat (drop 3 s)
 | take 3 s == ":t "         = if validGoal (drop 3 s)
                                then case p of
                                       Nothing     -> putStrLn "No program loaded." >> (interactiveLoop p strat file)
                                       (Just prog) -> putStrLn (show (sld prog (parseGoal (drop 3 s)))) >> (interactiveLoop p strat file)
                                 else putStrLn "The goal is invalid." >> interactiveLoop p strat file
 | s == ""                   = interactiveLoop p strat file
 | s == ";"                  = putStrLn "Type in a goal first to request more solutions." >> interactiveLoop p strat file
 | otherwise                 = putStrLn "Could not read the input. Type ':h' to get a list of valid commands." >> interactiveLoop p strat file


-- Called after the program solved a goal.
-- Reads user input in case the user wants to request additional solutions after.
requestSolution :: Maybe Prog -> Strategy -> Maybe String -> [Subst] -> IO ()
-- loaded program; current strategy; fileName; list of solutions
requestSolution p strat file (x:xs) = do
 s <- getLine
 if s == ";" then putStr (show x) >> requestSolution p strat file xs
   else
    if s == "" || s == "." then putStrLn "okay"
         >> interactiveLoop p strat file
    else putStrLn "Invalid input. Type ';' to request additional solutions or ENTER to accept the solution."
         >> requestSolution p strat file (x:xs)
requestSolution p strat file [] = do
 s <- getLine
 if s == ";" then putStrLn "No more solutions." >> interactiveLoop p strat file
   else
    if s == "" || s == "." then putStrLn "okay"
         >> interactiveLoop p strat file
      else putStrLn "Invalid input. Type ';' to request additional solutions or ENTER to accept the solution."
           >> requestSolution p strat file []


-- Parses the given string as a Goal.
-- Only to be used on strings that can be parsed without errors
parseGoal :: String -> Goal
parseGoal s = case (parse s) of
                Right g -> g
                Left _  -> error "Unexpected error"

-- Checks if the given string can be interpreted as a Goal
validGoal :: String -> Bool
validGoal s = case (parse s) :: Either String Goal of
               Left _  -> False
               Right _ -> True

-- Tries to read the file as a Prog and continues the loop in any case
readFileSave :: Maybe Prog -> Strategy -> String -> IO ()
readFileSave p strat file = do
  e <- (parseFile file) :: IO (Either String Prog)
  (case e of
    Left _            -> putStrLn "The file could not be found." >> interactiveLoop p strat (Just file)
    Right prog        -> putStrLn "Loaded." >> interactiveLoop (Just prog) strat (Just file))

-- String for the ":h" command
helpHeader :: String
helpHeader = unlines
 [
   " Commands available from the prompt:             "
 , "  <goal>      Solves/proves the specified goal.   "
 , "   :h          Shows this help message.            "
 , "   :l <file>   Loads the specified file.           "
 , "   :p          Prints the currently loaded program."
 , "   :q          Exits the interactive environment.  "
 , "   :r          Reloads the last loaded file.       "
 , "   :s <strat>  Sets the specified search strategy  "
 , "               where <strat> is one of 'dfs' or 'bfs'."
 , "   :t <goal>   Prints the SLD tree for the specified goal.  "
 ]

-- String for the start of the interactive program
header :: String
header = unlines
 [
   " Welcome!                               "
 , " Type :h   for help.                    "
 , "------------------------------------------"
 ]
