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
import Test.QuickCheck
import Substitutionen
import Variablen

ds :: Term -> Term -> Maybe(Term, Term)
-- | Determines the disagreement set of the two given terms
ds (Var (VarName "_")) _                        = Nothing
ds _ (Var (VarName "_"))                        = Nothing
ds (Var vn1) (Var vn2)
  | vn1 == vn2                                  = Nothing
  | otherwise                                   = Just (Var vn1, Var vn2)
ds (Comb n1 xs1) (Comb n2 xs2)
  | (Comb n1 xs1) == (Comb n2 xs2)              = Nothing
  | n1 /= n2 || length xs1 /= length xs2        = Just ((Comb n1 xs1), (Comb n2 xs2))
  | otherwise                                   = ds_helper xs1 xs2
    where
      ds_helper :: [Term] -> [Term] -> Maybe(Term, Term)
      -- | Determines the ds for two lists of terms.
      ds_helper ((Var v1):ts1) ((Var v2):ts2)
        | v1 == VarName "_" || v2 == VarName "_"  = ds_helper ts1 ts2
        | v1 == v2                                = ds_helper ts1 ts2
        | otherwise                               = Just(Var v1, Var v2)
      ds_helper ((Comb cn1 ct1):ts1) (((Comb cn2 ct2):ts2))
        | cn1 /= cn2 || length ct1 /= length ct2  = Just((Comb cn1 ct1), (Comb cn2 ct2))
        | ct1 /= ct2                              = let ds' = ds_helper ct1 ct2
                                                      in if ds' == Nothing
                                                        then ds_helper ts1 ts2
                                                        else ds'
        | otherwise                               = ds_helper ts1 ts2
      ds_helper ((Var v1):ts1) (t2:ts2)
        | v1 == VarName "_"                       = ds_helper ts1 ts2
        | otherwise                               = Just((Var v1),t2)
      ds_helper (t1:ts1) ((Var v2):ts2)
        | v2 == VarName "_"                       = ds_helper ts1 ts2
        | otherwise                               = Just(t1,(Var v2))
      ds_helper  []         []                    = Nothing
      ds_helper  _          _                     = error "Unexpected pattern."
ds t1 t2                                          = Just(t1,t2)

unify :: Term -> Term -> Maybe Subst
-- | Determines the most general unifier (mgu) if it exists
unify x y = unifyAcc x y empty
 where
   unifyAcc :: Term -> Term -> Subst -> Maybe Subst
   -- term 1; term 2; accumulated substitution
   unifyAcc t1 t2 s
      | ds (apply s t1) (apply s t2) == Nothing = Just s
      | otherwise =
          case (ds (apply s t1) (apply s t2)) of
            --1. Compose next single substitution with old substitution
            Just (Var v1, Var v2) -> unifyAcc t1 t2 (compose (single v1 (Var v2)) s)
            Just (Var v, t)       -> let s2 = (compose (single v t) s)
                                        --2. occur check
                                        in if (v `elem` (allVars t)) then Nothing else unifyAcc t1 t2 s2
            Just (t, Var v)       -> let s2 = (compose (single v t) s)
                                        in if (v `elem` (allVars t)) then Nothing else unifyAcc t1 t2 s2
            _                     -> Nothing

{- |
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}

prop_1 :: Term -> Bool
prop_1 t = ds t t == Nothing

prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = ds t1 t2 /= Nothing ==> t1 /= t2

prop_3 :: Term -> Bool
prop_3 t = case (unify t t) of
             Just s -> (unify t t /= Nothing && domain s == [])
             _      -> False

prop_4 :: Term -> Term -> Property
prop_4 t1 t2 = unify t1 t2 /= Nothing ==> case (unify t1 t2) of
                                            Just mgu -> ds (apply mgu t1 ) (apply mgu t2 ) == Nothing
                                            _        -> False

-- | return True if all tests were successful.
return []
-- | check all properties.
checkProperties :: IO Bool
checkProperties = $quickCheckAll
