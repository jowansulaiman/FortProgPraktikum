{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Unifikation
Description : Building on substitutions, the central operation of logic programming is now realized: unification.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'ds', $'unify'
The description of each function can be found below.
-}
module Unifikation
 --(domain, empty, single, apply, compose, restrictTo)
 where

import Type
import Data.List (nub, sort)
import PrettyPrinting
import Test.QuickCheck
import Substitutionen


{- data Term = Var VarName | Comb CombName [Term]
     deriving (Eq, Show)
-}

ds :: Term -> Term -> Maybe (Term, Term)
-- | the function 'ds', which calculates the disagreement set of two terms and returns them as a pair.
ds (Var (VarName x1))  (Var (VarName x2))
                        | x1 == x2  = Nothing
                        | otherwise = Just (Var (VarName x2),  (Var (VarName x2)))
ds (Comb f  m) (Comb g  n)
                        | (f /= g  && (length m) /= (length n))   = Just ((Comb f  m),  (Comb g n))
                        | (f == g  && (length m) == (length n))   =
                        | otherwise = Nothing

-- Comb CombName [Term]
-- | (single t1 (Var (VarName x2)) == x2 = Nothing
--                                        | otherwise Just ((Var (VarName x1)),  (Var (VarName x2)))
-- ds (Var (VarName x1))  (Var (VarName x2))  | x1 == x2 = Nothing
--                                     | otherwise = Just ((Var (VarName x1)),  (Var (VarName x2)))
{-
single :: VarName -> Term -> Subst
-- | a substitution, which simply maps x single variable to x term.
single varName term = Subst [(varName, term)]

apply :: Subst -> Term -> Term
-- |  The function 'apply', which applies x substitution to x term
apply (Subst []) term                                            = term
apply (Subst ((VarName x,y):xs)) (Var (VarName x1)) | x == x1    = y
                                                    | otherwise  = apply (Subst xs) (Var (VarName x1))
apply subst (Comb n v) = Comb n (map (apply subst) v)
-}
-- unify :: Term -> Term -> Maybe Subst
-- | the function 'unify', which, building on the function ds, determines the most general unifier for two terms,
-- provided that the two terms are unifiable.

{-|
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}
{-
prop_1 :: Term -> Bool
prop_1 t = ds(t,t) == []

prop_2 ::  Term ->  Term -> Property
prop_2 t1 t2  =  ds(t1,t2) /= []  ==>  t1 /= t2

prop_3 ::  Term ->  Term -> Property
prop_3 t1 t2  = ds(t1,t2) == [] ==> (unify(t1,t2) /= Nothing  && domain(unify(t1,t2)) == [])

prop_4 ::  Term ->  Term -> Property
prop_4 t1 t2  = unify(t1,t2) /=  Nothing  ==> (ds(apply(unify(t1,t2),t1),apply(unify(t1,t2),t2)) == [])


-- | return True, if it was successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
-}