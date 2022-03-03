{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Substitutionen
Description : Assignments of variables to terms as an abstract data type (ADT).
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains a type class $Subst,
which includes a method $'allVars' ::  x -> [VarName]
This method returns all variables
contained in a data type (without duplicates)..
-}

module Substitutionen
 (domain, empty, single, apply, compose, restrictTo)
 where
import Type
import Data.List (nub, sort)
import PrettyPrinting
import Test.QuickCheck
import Variablen

data Subst  = Subst [(VarName, Term)]
-- | Data type for program 'Subst'
  deriving (Show, Eq)

domain :: Subst -> [VarName]
-- | The function $'domain' returns the domain (Definitionsbereich) of a substitution.
domain (Subst []) = []
domain (Subst ((x,y):xs)) | helper (x,y)      = domain (Subst xs)
                          | otherwise         = nub (x:domain (Subst xs))
    -- | Checks if x VarName would be replaced by itself.
    where helper:: (VarName, Term) -> Bool
          helper (VarName x1, Var (VarName y1)) = x1 == y1
          helper (_,_)                          = False

empty :: Subst
-- | Creates an empty substitution.
empty = Subst []

single :: VarName -> Term -> Subst
-- | returns a substitution that only maps a single variable to a term.
single varName term = Subst [(varName, term)]

apply :: Subst -> Term -> Term
-- |  The function 'apply' applies a substitution to a term
apply (Subst []) term = term
apply (Subst ((VarName x,y):xs)) (Var (VarName x1)) | x == x1    = y
                                                    | otherwise = apply (Subst xs) (Var (VarName x1))
apply subst (Comb n v) = Comb n (map (apply subst) v)

compose :: Subst -> Subst -> Subst
-- | The function 'compose' composes two substitions with each other.
compose (Subst s1) (Subst s2) = Subst $ filter unRedundantSubstitution $ map (\(x,y) -> (x, apply (Subst s1) y)) s2
                                ++ filter (\(x,_) -> x `notElem` domain (Subst s2)) s1
  where
    unRedundantSubstitution :: (VarName,Term) -> Bool
    unRedundantSubstitution (_, Comb _ _) = True
    unRedundantSubstitution (x, Var y)    = x /= y

restrictTo :: Subst -> [VarName] -> Subst
-- | The function 'restrictTo' restricts the domain of a substitution to a given set of variables,
-- therefore eliminating the other sub-substitions
restrictTo (Subst s) n = Subst (filter (\(x,_) -> elem x n) s)

instance Pretty Subst where
-- | an instance of the type class Pretty to transform the substitutions into a more readable string.
    pretty (Subst []) = "{}"
    pretty (Subst s)  = "{" ++ helper s [] ++ "}"
        -- | Creates a string that lists all substitutions that would be made.
        -- The second argument is a list that contains all VarNames that have already been replaced.
        where helper :: [(VarName,Term)] -> [String] -> String
              helper [] _ = ""
              helper ((VarName x,y):xs) [] | x == pretty y = helper xs []
                                           | otherwise     = x ++ " -> " ++ pretty y ++ helper xs [x]
              helper ((VarName x,y):xs) l  | x == pretty y || elem x l = helper xs l
                                           | otherwise                 = ", " ++ x ++ " -> " ++ pretty y ++ helper xs (x:l)

instance Vars Subst where
  -- |  Instance for the predefined data type Subst
    allVars (Subst s) = nub $ foldr (++) []
                              (map (\(x,y) -> x:allVars y) (filter (\(VarName x,y) -> x /= pretty y) s))

instance Arbitrary Subst where
-- |  instance of Subst for the Arbitrary type class suitable for automated testing.
    arbitrary = do n <- choose (0,4)
                   x <- vector n `suchThat` (\x -> nub x == x)
                   y <- vector n
                   return $ Subst $ clean (zip x y)
        where clean :: [(VarName,Term)] -> [(VarName,Term)]
        -- | removes redundant substitutions
              clean [] = []
              clean ((VarName x,y):xs) | x == pretty y = clean xs
                                       | otherwise     = (VarName x,y):clean xs

isSubListOf :: [VarName] -> [VarName] -> Bool
-- | Checks if the first list is a sublist of the second list
isSubListOf xs ys = foldr (&&) True (map (\x -> elem x ys) xs)


{-|
--------------------------------------------{QuickCheck properties}-----------------------------------------------------
-}

prop_1 :: Term -> Bool
prop_1 t = apply empty t == t

prop_2 :: VarName -> Term -> Bool
prop_2 x t = apply (single x t) (Var x) == t

prop_3 :: Term -> Subst -> Subst -> Bool
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

prop_4 :: Bool
prop_4 = domain empty == []

prop_5 :: VarName -> Bool
prop_5 x = domain (single x (Var x)) == []

prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= Var x ==> domain (single x t) == [x]

prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = isSubListOf (domain (compose s1 s2)) (domain s1 ++ domain s2)

prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 = x1 /= x2 ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2))) == [x2]

prop_9 :: Bool
prop_9 = allVars empty == []

prop_10 :: VarName -> Bool
prop_10 x = allVars (single x (Var x)) == []

prop_11 :: VarName -> Term -> Property
prop_11 x t = t /= Var x ==> sort (allVars (single x t)) == sort (nub $ (allVars t) ++ [x])

prop_12 :: Subst -> Subst -> Bool
prop_12 s1 s2 = isSubListOf (allVars (compose s1 s2)) (allVars s1 ++ allVars s2)

prop_13 :: VarName -> VarName -> Property
prop_13 x1 x2 = x1 /= x2 ==> sort (allVars (compose (single x2 (Var x1)) (single x1 (Var x2)))) == sort [x1, x2]

prop_14 :: Subst -> Bool
prop_14 s = isSubListOf (domain s) (allVars s)

prop_15 :: [VarName] -> Bool
prop_15 xs = domain (restrictTo empty xs) == []

prop_16 :: [VarName] -> Subst -> Bool
prop_16 xs s = isSubListOf (domain (restrictTo s xs)) xs

-- Tests all properties.
return []
testAll :: IO Bool
testAll = $quickCheckAll
