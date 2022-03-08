{-|
Module      : InteraktiveUmgebung
Description : Ithe interactive environment, which you can use in the form of a REPL (Read-Eval-Print-Loop)
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'rename'
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

interaktive :: IO ()
interaktive = do
  putStr header
  interaktiveLoop Nothing bfs Nothing

interaktiveLoop ::  Maybe Prog -> Strategy -> Maybe String -> IO ()
interaktiveLoop p strat file = do
 putStr "?- "
 s <- getLine
 processInput s p strat file

processInput :: String -> Maybe Prog -> Strategy -> Maybe String -> IO ()
processInput s p strat file
  | s == ":h"                 = putStrLn helpHeader >> (interaktiveLoop p strat file)
  | s == ":p"                 =  case p of
                                  Nothing     -> putStrLn "No program loaded." >> (interaktiveLoop p strat file)
                                  (Just prog) -> putStrLn "Loaded program:" >> putStrLn (pretty prog) >> (interaktiveLoop p strat file)
  | s == ":s dfs"             = putStrLn "Strategy set to depth-first search." >> interaktiveLoop p dfs file
  | s == ":s bfs"             = putStrLn "Strategy set to breadth-first search." >> interaktiveLoop p bfs file
  | s == ":q"                 = putStrLn "Successful termination."
  | validGoal s = case p of
                   Nothing     -> putStrLn "No program loaded." >> (interaktiveLoop p strat file)
                   (Just prog) -> putStrLn (show (sld prog (parseGoal s))) >> (interaktiveLoop p strat file)
  -- | s == ":r"                 = case file of
  --                                Nothing -> putStrLn "There is no last loaded file" >> interaktiveLoop p strat file
  --                                Just f  -> readFileSave p strat f
  -- | s == ":l " ++ fileName    = readFileSave p strat fileName
  | take 3 s == ":t "            = if validGoal (drop 3 s)
                                    then case p of
                                          Nothing     -> putStrLn "No program loaded." >> (interaktiveLoop p strat file)
                                          (Just prog) -> putStrLn (show (sld prog (parseGoal (drop 3 s)))) >> (interaktiveLoop p strat file)
                                    else putStrLn "The goal is invalid." >> interaktiveLoop p strat file
  | otherwise                    = putStrLn "Could not read the input. Please try again." >> interaktiveLoop p strat file


parseGoal :: String -> Goal
parseGoal s = case (parse s) of
               Right g -> g
               Left _  -> error "Unexpected error"


validGoal :: String -> Bool
validGoal s = case (parse s) :: Either String Goal of
              Left _  -> False
              Right _ -> True

-- UNVOLLSTÄNDIG
-- readFileSave :: Maybe Prog -> Strategy -> String
-- readFileSave p strat s = parseFile ...

-- optionQuestion :: String -> IO ()
-- optionQuestion question = do
--  putStr (question ++ "?- ")
--  c <- getLine
--  case c of
--    ":h" -> putStrLn  helpHeader       >> optionQuestion question
--    ":q" -> return ()
--    _   -> putStrLn "Invalid input!" >> optionQuestion question

-- unMaybe :: Maybe a -> a
-- unMaybe (Just v)  = v
-- unMaybe Nothing = error "Can't transform Nothing"


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

header :: String
header = unlines
  [
    " Welcome!                               "
  , " Type :h   for help.                    "
  , "------------------------------------------"
  ]
