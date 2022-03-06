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
import Data.List (nub, sort)
import PrettyPrinting
import Test.QuickCheck
import Variablen

-- Resolutionsprinzip
{-
data Term = Var VarName | Comb CombName [Term]
data Rule = Rule Term [Term]
-}
rename :: [VarName] -> Rule -> Rule
-- | the function $'rename', which renames the variables of a rule.
rename [] rule                   = rule
rename [v]    (Rule (Var v1) []) = (Rule (Var v) [])
-- rename (x:xs) (Rule t []) = (Rule v [])


intersect :: [VarName] -> [VarName] -> [VarName]
-- | intersection of two lists of VarName
intersect l r = nub (helper l r)
 where
  helper [] _                 = []
  helper (x:xs) l | elem x l  = x : helper xs l
                  | otherwise = helper xs l

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

-- | return True, if it was successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
