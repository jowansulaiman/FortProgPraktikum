{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Umbenennung
Description : In preparation for SLD resolution, variants of the rules present in the program are now generated.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'rename'
The description of each function can be found below.
-}
module Umbenennung
 --(domain, empty, single, apply, compose, restrictTo)
 where

import Type
import Data.List (nub)
import PrettyPrinting
import Test.QuickCheck
import Variablen
import Substitutionen


rename :: [VarName] -> Rule -> Rule
-- | Renames the rule using the freshVars function but ommitung the VarNames
-- in the first argument
rename l (Rule t ts) = let (y:ys) = renameTerms (l ++ (allVars t ++ (concatMap allVars ts)))
                                     (allVars t ++ (concatMap allVars ts)) (t:ts) freshVars
                         in Rule y ys
 where
  -- Renames the list of terms by iterating through (v:vs) using the names given
  -- in nextNames but ommiting the VarNames given in f.
  renameTerms :: [VarName] -> [VarName] -> [Term] -> [VarName] -> [Term]
  --             forbidden     remaining   terms     nextNames
  renameTerms f  []    ts ns   = renameAll_ f ts ns
  renameTerms f (v:vs) ts (n:ns)
   | n `elem` f                = renameTerms f (v:vs) ts ns
   | v == VarName "_"          = renameTerms f vs ts (n:ns)
   | otherwise                 = renameTerms f vs (map (apply (single v (Var n))) ts) ns

  renameAll_ :: [VarName] -> [Term] -> [VarName] -> [Term]
  -- | Renames every Variable "_"
  renameAll_ f ts (n:ns)
   | n `elem` f = renameAll_ f ts ns
   | otherwise  = let new = rename_ ts n
                    in if new == ts then ts else renameAll_ f new ns

  rename_ :: [Term] -> VarName -> [Term]
  -- | renames the first occurence of VarName "_" to the given VarName
  rename_ ((Var vn):ts) v
   | vn == VarName "_" = (Var v):ts
   | otherwise         = (Var vn):(rename_ ts v)
  rename_ ((Comb cn xs):ts) v
   | (VarName "_") `elem` (allVars (Comb cn xs)) = (Comb cn (rename_ xs v)):ts
   | otherwise                                   = (Comb cn xs):(rename_ ts v)
  rename_ [] _ = []

intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = helper (nub xs) ys
 where
  helper [] _ = []
  helper (x:xs) ys
   | x `elem` ys = x:(intersect xs ys)
   | otherwise   = intersect xs ys

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

-- | return True if all tests are successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
