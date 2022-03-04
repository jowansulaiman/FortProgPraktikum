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

rename :: [VarName] -> Rule -> Rule
rename xs rule = rule
-- | the function $'rename', which renames the variables of a rule.

{-|
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}
-- intersect


prop_1 :: [VarName] -> Term -> Bool
prop_1 xs r = intersect allVars(rename(xs,r))  allVars(r) == []

prop_2 :: [VarName] -> Term -> Bool
prop_2 xs r = intersect allVars(rename(xs,r))  xs         == []

prop_3 :: [VarName] -> Term -> Bool
prop_3 xs r = ("_" `notElem` allVars(rename(xs,r)))         == []

prop_4 :: [VarName] -> Term -> Property
prop_4 xs r = "_" `notElem` allVars(r) ==>  length(allVars(rename(xs,r))) == length (allVars(r))

prop_5 :: [VarName] -> Term -> Bool
prop_5 xs r =  length(allVars(rename(xs,r))) >= length (allVars(r))


-- | return True, if it was successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll