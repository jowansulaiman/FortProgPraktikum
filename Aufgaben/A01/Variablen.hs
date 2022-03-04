{-|
Module      : Variablen
Description : determining existing variable names and creating an infinite amount of fresh ones
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains the type class $Vars
which contains the function $'allVars'.
The module also contains the function $freshVars.
The description of each function can be found below.
-}
module Variablen
  where
import A01.Type
import Data.Char
import Data.List (nub)

class (Show a) =>  (Vars a)  where
   -- | default function
   allVars ::  a -> [VarName]
   -- | The function $'allVars' returns all variables contained in a data type (without duplicates).
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

freshVars :: [VarName]
-- | Returns an infinite List of Varnames valid in Prolog. (specified in task 3.2)
freshVars = genFreshVars 0
 where
 genFreshVars :: Int -> [VarName]
 genFreshVars n | n < 26    = (VarName [chr(ord('A')+n)]):(genFreshVars (n+1))
                | n >= 26   = (VarName ([chr(ord('A')+(mod n 26))] ++ (show ((div n 26)-1)))):(genFreshVars (n+1))
                | otherwise = []
