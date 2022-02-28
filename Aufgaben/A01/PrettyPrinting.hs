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
   pretty  (Comb z x)        = z ++ "(" ++ commaSep (getTerm x) ", " ++ ")"

instance Pretty Rule where
-- | Instances for the predefined data types Rule
   pretty  (Rule (Var  (VarName x)) []) = x
   pretty  (Rule (Comb y []) []       ) =  y
   pretty  (Rule (Comb y x ) []       ) =  y ++ "(" ++ commaSep (getTerm x) ", "        ++ ")."
   pretty  (Rule (Comb y x ) z        ) = (y ++ "(" ++ commaSep (getTerm x) ", "       ++ ") :- ")    ++ commaSep (getTerm z) ", " ++ "."

instance Pretty Prog where
-- | Instances for the predefined data types Prog
   pretty  (Prog []) = ""
   pretty  (Prog x) = commaSep (getTerm x) "\n "

instance Pretty Goal where
-- | Instances for the predefined data types Goal
  pretty  (Goal []) = "?- ."
  pretty  (Goal x ) = "?- " ++ commaSep (getTerm x) ", "  ++ ")."

getTerm :: (Pretty a) => [a] -> [String]
-- | The function 'getTerm' converts a list of a into a list of string using pretty.
getTerm [] = []
getTerm (x:xs) = [pretty x]  ++ getTerm xs

commaSep :: [String] -> String -> String
-- | The function 'commaSep' Transform a list of strings into a comma separated string
commaSep [] _ = ""
commaSep s  k = foldr1 (\s1 s2 -> s1 ++ k ++ s2) s
