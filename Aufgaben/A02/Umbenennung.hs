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
-- | the function $'rename', which renames the variables of a rule.

{-|
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}
