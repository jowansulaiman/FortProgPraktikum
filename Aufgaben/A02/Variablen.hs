{-|
Module      : Variablen
Description : determining existing variable names and creating an infinite amount of fresh ones
Maintainer  : Jowan Sulaiman and Kjell Rothenburger
the module contains a type class $Vars,
which includes a method $'allVars' ::  a -> [VarName] This method returns all variables
contained in a data type (without duplicates).
and a function $freshVars which generates an infinite number of fresh variable names
-}
module Variablen where
import Type
import Data.Char
import Data.List (nub)

class (Show a) =>  (Vars a)  where
   -- ^ default function
   allVars ::  a -> [VarName]
   -- | This function $'allVars' returns all variables contained in a data type (without duplicates).
   allVars   x      = [VarName (show x)]

instance Vars Term where
-- | Instance for the predefined data type Term
   allVars  (Var (VarName x)) = [VarName x]
   allVars  (Comb _ []  )     = []
   allVars  (Comb _ (xs))     = nub (concat (map allVars xs))

instance Vars Rule where
-- | Instance for the predefined data type Rule
   allVars  (Rule t []) = allVars t
   allVars  (Rule t ts) = nub $ allVars t ++ concat (map allVars ts)

instance Vars Prog where
-- | Instance for the predefined data type Prog
   allVars  (Prog [])    = []
   allVars  (Prog rules) = nub (concat (map allVars rules))

instance Vars Goal where
-- | Instance for the predefined data type Goal
  allVars (Goal [])  = []
  allVars (Goal ts ) = nub (concat (map allVars ts))

-- Returns an infinite List of Varnames as specified in task 3.2
freshVars :: [VarName]
freshVars = genFreshVars 0
 where
 genFreshVars :: Int -> [VarName]
 genFreshVars n | n < 26  = (VarName [chr(ord('A')+n)]):(genFreshVars (n+1))
                | n >= 26 = (VarName ([chr(ord('A')+(mod n 26))] ++ (show ((div n 26)-1)))):(genFreshVars (n+1))
                | otherwise = []
