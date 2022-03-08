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

interaktive :: IO ()
interaktive = do
  putStr header

interaktiveLoop ::  IO ()
interaktiveLoop  = do      putStr header
                           optionQuestion ""

optionQuestion :: String -> IO ()
optionQuestion question = do
  putStr (question ++ "?- ")
  c <- getLine
  case c of
    ":h" -> putStrLn  helpHeader       >> optionQuestion question
    ":q" -> return ()
    _   -> putStrLn "Invalid input!" >> optionQuestion question


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