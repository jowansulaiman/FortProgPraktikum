{-|
Module      : InteraktiveUmgebung
Description : An interactive environment for the prolog interpreter
Maintainer  : Jowan Sulaiman and Kjell Rothenburger
The module contains the following functions $'start', $'interactiveLoop',
$'processInput', $'parseGoal', $'validGoal', $'readFileSave', $'helpHeader and
'$'header'.
The description of each function can be found below.
-}
module InteraktiveUmgebung
 where

import Type
import Data.List (nub, sort)
import Substitutionen
import Test.QuickCheck
import Variablen
import Unifikation
import Umbenennung
import Data.Maybe(isNothing)
import System.IO
import Control.Monad
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
                                 (Just prog) -> putStrLn (show (solveWith prog (parseGoal s) strat)) >> (interactiveLoop p strat file)
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
 | otherwise                 = putStrLn "Could not read the input. Please try again." >> interactiveLoop p strat file


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
    Left s            -> putStrLn "The file could not be found." >> interactiveLoop p strat (Just file)
    Right prog        -> putStrLn "Loaded." >> interactiveLoop (Just prog) strat (Just file))

-- String for the ":h" command
helpHeader :: String
helpHeader = unlines
 [
   " <goal>      Solves/proves the specified goal.   "
 , " :h          Shows this help message.            "
 , " :l <file>   Loads the specified file.           "
 , " :p          Prints the currently loaded program."
 , " :q          Exits the interactive environment.  "
 , " :r          Reloads the last loaded file.       "
 , " :s <strat>  Sets the specified search strategy  "
 , "             where <strat> is one of 'dfs', 'bfs', or 'iddfs'."
 , " :t <goal>   Prints the SLD tree for the specified goal.  "
 ]

-- String for the start of the interactive program
header :: String
header = unlines
 [
   " Welcome!                               "
 , " Type :h   for help.                    "
 , "------------------------------------------"
 ]