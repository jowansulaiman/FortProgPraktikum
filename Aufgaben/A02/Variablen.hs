{-|
Module      : Variablen
Description : determine existing variable names as well as create fresh variable names

Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains a type class $Vars,
which includes a method $'allVars' ::  a -> [VarName] This method returns all variables
contained in a data type (without duplicates)..
-}
module Variablen where
import Type
import Data.Char

class (Show a) =>  (Vars a)  where
   -- ^ default function
   allVars ::  a -> [VarName]
   -- | This function $'allVars' returns all variables contained in a data type (without duplicates).
   allVars   x      = [VarName (show x)]

instance Vars Term where
-- | Instances for the predefined data type Term
   allVars  (Var (VarName x)) = [VarName x]
   allVars  (Comb _ []  )     = [] --[VarName f]
   allVars  (Comb _ (xs))     = nub (concat (map allVars xs)) --concat (map allVars xs) --concat (map allVars xs)

instance Vars Rule where
-- | Instances for the predefined data type Rule
   allVars  (Rule t [])       = allVars t
   allVars  (Rule t s)        = allVars  t  ++ nub (concat (map allVars s))

instance Vars Prog where
-- | Instances for the predefined data type Prog
   allVars  (Prog [])         = []
   allVars  (Prog x)          = nub (concat (map allVars x))

instance Vars Goal where
-- | Instances for the predefined data type Goal
  allVars (Goal [])           = []
  allVars (Goal x )           = nub (concat (map allVars x))


nub :: Eq a => [a] -> [a]
-- | Delete all duplicates in a list
nub []                        = []
nub (x:xs)                    = x : nub (filter (/=x) xs)

-- Returns an infinite List of Varnames as specified in task 3.2
freshVars :: [VarName]
freshVars = genFreshVars 0
 where
 genFreshVars :: Int -> [VarName]
 genFreshVars n | n < 26  = (VarName [chr(ord('A')+n)]):(genFreshVars (n+1))
                | n >= 26 = (VarName ([chr(ord('A')+(mod n 26))] ++
                (show ((div n 26)-1)))):(genFreshVars (n+1))
