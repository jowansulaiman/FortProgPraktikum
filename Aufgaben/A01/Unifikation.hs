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


ds :: Term -> Term -> Maybe (Term, Term)
-- | Determines the disagreement set of the two given terms
ds (Var (VarName "_")) _                              = Nothing
ds _ (Var (VarName "_"))                              = Nothing
ds (Var (VarName v1)) (Var (VarName v2))
    | v1 == v2                                        = Nothing
    | otherwise                                       = Just (Var (VarName v1),Var (VarName v2))
ds (Comb cn1 []) (Comb cn2 [])
    | cn1 == cn2                                      = Nothing
    | otherwise                                       = Just (Comb cn1 [],Comb cn2 [])
ds (Comb cn1 (v1:xs)) (Comb cn2 (v2:ys))
    | cn1 /= cn2 || length (v1:xs) /= length (v2:ys)  = Just (Comb cn1 (v1:xs),Comb cn1 (v2:ys))
    | ds v1 v2 /= Nothing                             = ds v1 v2
    | otherwise                                       = ds (Comb cn1 xs) (Comb cn2 ys)
ds v1 v2 = Just (v1,v2)

{- |
ds :: Term -> Term -> Maybe(Term, Term)
ds (Var vn1) (Var vn2)           | vn1 == vn2                               = Nothing
                                 | otherwise                                = Just (Var vn1, Var vn2)
ds (Comb cn1 ts1) (Comb cn2 ts2) | (Comb cn1 ts1) == (Comb cn2 ts2)         = Nothing
                                 | cn1 /= cn2 && length ts1 /= length ts2   = Just ((Comb cn1 ts1), (Comb cn2 ts2))
                                -- | cn1 /= cn2 && length ts1 ==0 &&  length ts2 == 0 = Nothing
                                 | otherwise                                = ds_helper ts1 ts2
                                   where
                                      ds_helper :: [Term] -> [Term] -> Maybe(Term, Term)
                                      ds_helper ((Var v1):ts1) ((Var v2):ts2) | v1 == v2  = ds_helper ts1 ts2
                                                                              | otherwise = Just(Var v1, Var v2)
                                      ds_helper ((Comb cn1 ct1):ts1) (((Comb cn2 ct2):ts2)) | cn1 /= cn2               = Just((Comb cn1 ct1), (Comb cn2 ct2))
                                                                                            | length ct1 /= length ct2 = Just((Comb cn1 ct1), (Comb cn2 ct2))
                                                                                            | ct1 /= ct2               = ds_helper ct1 ct2
                                                                                            | otherwise                = ds_helper ts1 ts2
                                      ds_helper (t1:ts1) (t2:ts2) = Just(t1,t2)
                                      ds_helper  []         []    = Nothing
ds t1 t2 = Just(t1,t2)

 -------------------  { Test results} ------------------------------

=== prop_1 from Unifikation.hs:65 ===
+++ OK, passed 100 tests.

=== prop_2 from Unifikation.hs:68 ===
*** Failed! (after 2 tests):
Exception:
  Unifikation.hs:(35,39)-(42,75): Non-exhaustive patterns in function ds_helper
Comb "f" []
Comb "f" [Var (VarName "_")]

=== prop_3 from Unifikation.hs:75 ===
+++ OK, passed 100 tests.

=== prop_4 from Unifikation.hs:80 ===
*** Failed! (after 2 tests):
Exception:
  Unifikation.hs:(35,39)-(42,75): Non-exhaustive patterns in function ds_helper
Comb "g" [Comb "g" [],Comb "g" [Comb "g" [Var (VarName "_0"),Var (VarName "_")]]]
Comb "g" []

False
-}

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



domainMaybe :: Maybe Subst -> [VarName]
-- | Applies domain to an instance of maybe.
domainMaybe (Just v1)   = domain v1
domainMaybe _           = []

applyMaybe :: Maybe Subst -> Term -> Term
-- | Applies apply to an instance of maybe.
applyMaybe (Just v1) t  = apply v1 t
applyMaybe _        _   = Var (VarName "Fehler")
{- |
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}

prop_1 :: Term -> Bool
prop_1 t = ds t t == Nothing

prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2

prop_3 :: Term -> Term -> Property
prop_3 t1 t2 = ds t1 t2 == Nothing ==>  (unify t1 t2) /= Nothing  && domainMaybe (unify t1 t2) == []

prop_4 :: Term -> Term -> Property
prop_4 t1 t2 = (unify t1 t2) /= Nothing ==> ds (applyMaybe (unify t1 t2) t1) (applyMaybe (unify t1 t2) t2) == Nothing

-- | return True, if it was successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
