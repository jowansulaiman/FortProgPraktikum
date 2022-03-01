{-|
Module      : PrettyPrinting
Description : Representation of terms

Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains a type class $Pretty,
which includes a method $'pretty' :: a -> String to output data types nicely.
-}
module PrettyPrinting where
import Type

class  (Show a) => (Pretty a)  where
     -- ^ The 'a' argument and The 'String' is return value
     pretty ::a -> String
     -- | The function $'pretty' takes a value and converts it to string with the help of show.
     pretty  = show

instance Pretty Term where
-- | Instances for the predefined data types Term
   pretty  (Var (VarName x)) = x
   pretty  (Comb z [])       = z
   pretty  (Comb z x)        = z ++ "(" ++ commaSep (map pretty x) ", " ++ ")"

instance Pretty Rule where
-- | Instances for the predefined data types Rule
   pretty  (Rule t []) = pretty  t  ++ "."
   pretty  (Rule t s)  = pretty  t  ++ " :- " ++ commaSep (map pretty s) ", "  ++ "."

instance Pretty Prog where
-- | Instances for the predefined data types Prog
   pretty  (Prog []) = ""
   pretty  (Prog x) = commaSep (map pretty x)  "\n "

instance Pretty Goal where
-- | Instances for the predefined data types Goal
  pretty  (Goal []) = "?- ."
  pretty  (Goal x ) = "?- " ++ commaSep (map pretty x)  ", "  ++ "."

commaSep :: [String] -> String -> String
-- | The function 'commaSep' Transform a list of strings into a comma separated string
commaSep [] _ = ""
commaSep s  k = foldr1 (\s1 s2 -> s1 ++ k ++ s2) s

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _    _    = []
replace s  find repl =
     if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
