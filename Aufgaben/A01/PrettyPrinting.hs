{-|
Module      : PrettyPrinting
Description : Representation of terms

Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains a type class $Pretty which contains  the function $'pretty'.
The module also contains the function $concatSep.
The description of each function can be found below..
-}

module PrettyPrinting where
import Type

class  (Show a) => (Pretty a)  where
     -- | default function
     pretty :: a -> String
     -- | The function $'pretty' takes a value and converts it to a string by using show.
     pretty  = show

instance Pretty Term where
-- | Instance for the predefined data type Term
   pretty  (Var (VarName x)) = x
   pretty  (Comb z [])       = z
   pretty  (Comb z x)        = z ++ "(" ++ concatSep (map pretty x) ", " ++ ")"

instance Pretty Rule where
-- | Instance for the predefined data type Rule
   pretty  (Rule t []) = pretty  t  ++ "."
   pretty  (Rule t s)  = pretty  t  ++ " :- " ++ concatSep (map pretty s) ", "  ++ "."

instance Pretty Prog where
-- | Instance for the predefined data type Prog
   pretty  (Prog []) = ""
   pretty  (Prog x) = concatSep (map pretty x)  "\n"

instance Pretty Goal where
-- | Instances for the predefined data types Goal
  pretty  (Goal []) = "?- ."
  pretty  (Goal x ) = "?- " ++ concatSep (map pretty x)  ", "  ++ "."

concatSep :: [String] -> String -> String
-- | The function concatSep concatenates a list of strings to a single string
-- while also seperating the elements using the given seperator
concatSep [] _   = ""
concatSep ss sep = foldr1 (\s1 s2 -> s1 ++ sep ++ s2) ss

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _    _    = []
replace s  find repl =
     if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
