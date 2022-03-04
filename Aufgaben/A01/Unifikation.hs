{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Unifikation
Description : Building on substitutions, the central operation of logic programming is now realized: unification.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the functions $'ds' and $'unify'
The description of each function can be found below.
-}
module Unifikation
 where

import Type
import Data.List (nub, sort)
import PrettyPrinting
import Test.QuickCheck
import Substitutionen
import Variablen


{- data Term = Var VarName | Comb CombName [Term]
     deriving (Eq, Show)
-}

ds :: Term -> Term -> Maybe(Term, Term)
-- | Determines the disagreement set of the two given terms
ds (Var vn1) (Var vn2)           | vn1 == vn2 = Nothing
                                 | otherwise  = Just (Var vn1, Var vn2)
ds (Comb cn1 ts1) (Comb cn2 ts2) | (Comb cn1 ts1) == (Comb cn2 ts2) = Nothing
                                 | cn1 /= cn2                       = Just ((Comb cn1 ts1), (Comb cn2 ts2))
                                 | length ts1 /= length ts2         = Just ((Comb cn1 ts1), (Comb cn2 ts2))
                                 | otherwise                        = ds_helper ts1 ts2
                                   where
                                      ds_helper :: [Term] -> [Term] -> Maybe(Term, Term)
                                      ds_helper ((Var v1):ts1) ((Var v2):ts2) | v1 == v2  = ds_helper ts1 ts2
                                                                              | otherwise = Just(Var v1, Var v2)
                                      ds_helper ((Comb cn1 ct1):ts1) (((Comb cn2 ct2):ts2)) | cn1 /= cn2               = Just((Comb cn1 ct1), (Comb cn2 ct2))
                                                                                            | length ct1 /= length ct2 = Just((Comb cn1 ct1), (Comb cn2 ct2))
                                                                                            | ct1 /= ct2               = ds_helper ct1 ct2
                                                                                            | otherwise                = ds_helper ts1 ts2
                                      ds_helper (t1:ts1) (t2:ts2) = Just(t1,t2)
                                      ds_helper  []         []    = (error "Unexpected case")
ds t1 t2 = Just(t1,t2)


unify :: Term -> Term -> Maybe Subst
-- | Determines the most general unificator if it exists
unify t1 t2 = unifyAcc t1 t2 empty
 where
   unifyAcc :: Term -> Term -> Subst -> Maybe Subst
   unifyAcc t1 t2 s | ds (apply s t1) (apply s t2) == Nothing = Just s
                    | otherwise = case (ds (apply s t1) (apply s t2)) of
                                    --1. Compose next single substitution with old substitution
                                    Just (Var v1, Var v2) -> unifyAcc t1 t2 (compose (single v1 (Var v2)) s)
                                    Just (Var v, t)       -> let s2 = (compose (single v t) s)
                                    --2. occur check
                                                              in if (v `elem` (allVars t)) then Nothing else unifyAcc t1 t2 s2
                                    Just (t, Var v)       -> let s2 = (compose (single v t) s)
                                                              in if (v `elem` (allVars t)) then Nothing else unifyAcc t1 t2 s2
                                    _                     -> Nothing



--------------------------------------------{QuickCheck properties}-----------------------------------------------------
prop_1 :: Term -> Bool
prop_1 t = ds t t == Nothing

prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2

-- prop_3 :: Term -> Term -> Property
-- prop_3 t1 t2 = ds t1 t2 == Nothing ==> case (unify t1 t2) of
--                                           Just s -> (unify t1 t2 /= Nothing && domain s == [])
--                                           _      -> False
prop_3 :: Term -> Bool
prop_3 t = case (unify t t) of
             Just s -> (unify t t /= Nothing && domain s == [])
             _      -> False

prop_4 :: Term -> Term -> Property
prop_4 t1 t2 = unify t1 t2 /= Nothing ==> case (unify t1 t2) of
                                            Just mgu -> ds (apply mgu t1 ) (apply mgu t2 ) == Nothing
                                            _        -> False

-- Tests all properties.
return []
checkProperties :: IO Bool
checkProperties = $quickCheckAll
