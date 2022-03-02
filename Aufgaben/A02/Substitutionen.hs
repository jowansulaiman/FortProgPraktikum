{-|
Module      : Substitutionen
Description : Assignments of variables are realized to terms, as an abstract data type (ADT).
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

the module contains a type class $Subst,

which includes a method $'allVars' ::  a -> [VarName] This method returns all variables
contained in a data type (without duplicates)..
-}

module Substitutionen where
import Type

data Subst  = Empty | Subst [(VarName, Term)]
-- | Data type for program 'Subst'
  deriving (Show, Eq)

domain :: Subst -> [VarName]
-- |  The function $'domain' that returns the definition domain of a substitution.
domain Empty                     =  []
domain (Subst ((x, (Var y)):xs)) =  if x == y then domain (Subst xs)
                                    else x : domain (Subst xs)
domain (Subst ((x, _):xs))       =  x : domain (Subst xs)

empty :: Subst
-- | Creating an empty substitution.
empty = Empty

single :: VarName -> Term -> Subst
-- | a substitution, which simply maps a single variable to a term.
single varName term = Subst [(varName, term)]

apply :: Subst -> Term -> Term
-- |  The function 'apply', which applies a substitution to a term
apply empty  term                     = term
apply (Subst ((v, x) :xs))   (Var t)  = if v == t then x else Var t
--apply subst  (Comb combName (t:ts))   = (foldr  apply subst ts) -- map (apply subst) ts

compose :: Subst -> Subst -> Subst
compose empty s     = s
compose s     empty = s
compose Subst ((v, Var v1): xs)     Subst ((v2, Var v3): ys)   =  if v1 == v2 && v == v3 then empty
                                                             else if v1 == v2 && v != v3 then Subst ((v1, Var v3))
                                                             else

-- Subst [(VarName, Term)]
-- Var VarName | Comb CombName [Term]

