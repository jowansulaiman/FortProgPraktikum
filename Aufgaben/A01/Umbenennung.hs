{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Umbenennung
Description : In preparation for SLD resolution, variants of the rules present in the program are now generated.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'rename'
The description of each function can be found below.
-}

module Umbenennung
  where
import Type
import Data.List (nub)
import Test.QuickCheck
import Variablen
import Substitutionen


rename :: [VarName] -> Rule -> Rule
-- | Renames the rule using the freshVars function but ommitung the VarNames
-- in the first argument
rename l (Rule z zs) = let (y:ys) = renameTerms (l ++ (allVars z ++ (concatMap allVars zs)))
                                     (allVars z ++ (concatMap allVars zs)) (z:zs) freshVars
                         in Rule y ys
 where
  -- Renames the list of terms by iterating through (v:vs) using the names given
  -- in nextNames but ommiting the VarNames given in f.
  renameTerms :: [VarName] -> [VarName] -> [Term] -> [VarName] -> [Term]
  -- forbidden names; remaining names; all terms; nextNames
  renameTerms f  []    xs ns   = renameAll_ f xs ns
  renameTerms f (v:vs) xs (n:ns)
   | n `elem` f                = renameTerms f (v:vs) xs ns
   | v == VarName "_"          = renameTerms f vs xs (n:ns)
   | otherwise                 = renameTerms f vs (map (apply (single v (Var n))) xs) ns
  renameTerms _  _    _ _      = error "Unexpected pattern."

  -- Renames every Variable "_" by calling rename_ until there is no anonymous
  -- variable left
  renameAll_ :: [VarName] -> [Term] -> [VarName] -> [Term]
  -- forbidden names; all terms; nextNames
  renameAll_ f ts (n:ns)
   | n `elem` f    = renameAll_ f ts ns
   | otherwise     = let new = rename_ ts n
                       in if new == ts then ts else renameAll_ f new ns
  renameAll_ _ _ _ = error " "

  -- renames the first occurence of VarName "_" to the given VarName
  rename_ :: [Term] -> VarName -> [Term]
  -- all terms; new name
  rename_ ((Var vn):ts) v
   | vn == VarName "_" = (Var v):ts
   | otherwise         = (Var vn):(rename_ ts v)
  rename_ ((Comb cn xs):ts) v
   | (VarName "_") `elem` (allVars (Comb cn xs)) = (Comb cn (rename_ xs v)):ts
   | otherwise                                   = (Comb cn xs):(rename_ ts v)
  rename_ [] _ = []

intersect :: [VarName] -> [VarName] -> [VarName]
-- | intersection of two lists of VarName
intersect l r = nub (helper l r)
 where
  helper [] _                  = []
  helper (x:xs) ls | elem x ls = x : helper xs ls
                   | otherwise = helper xs ls

--------------------------------------------{QuickCheck properties}-----------------------------------------------------

prop_1 :: [VarName] -> Rule -> Bool
prop_1 xs r = intersect (allVars(rename xs r))  (allVars(r)) == []

prop_2 :: [VarName] -> Rule -> Bool
prop_2 xs r = intersect (allVars(rename xs r))  xs        == []

prop_3 :: [VarName] -> Rule -> Bool
prop_3 xs r = (VarName "_") `notElem` (allVars(rename xs r ))

prop_4 :: [VarName] -> Rule -> Property
prop_4 xs r = (VarName "_") `notElem` allVars(r) ==>  length(allVars(rename xs r )) == length (allVars(r))

prop_5 :: [VarName] -> Rule -> Bool
prop_5 xs r =  length(allVars(rename xs r )) >= length (allVars(r))

-- | return True if all tests were successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
