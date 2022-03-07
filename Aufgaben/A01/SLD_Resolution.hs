{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : SLD_Resolution
Description : SLD trees for evaluating queries becomes a suitable representation.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'rename'
The description of each function can be found below.
-}
module SLD_Resolution
  where

import Type
import Data.List (nub, sort)
import Substitutionen
import Test.QuickCheck
import Variablen
import Unifikation
import Umbenennung
import Data.Maybe(isNothing)

data SLDTree = Node Goal [(Subst,SLDTree)]
-- | Data type 'SLD trees'
  deriving Show

sld :: Prog -> Goal -> SLDTree
-- |  the function 'sld', which constructs the SLD tree to a program and a request.
sld (Prog p) g  = Node g (helper (allVars (Prog p) ++ allVars g) p (Prog p) g)
  -- | return each subtree
  where helper :: [VarName] -> [Rule] -> Prog -> Goal -> [(Subst,SLDTree)]
        helper _ _ _ (Goal [])      = [] -- Leaf
        helper _ _ (Prog []) _      = [] -- Fail
        helper _ [] _ _             = []
        helper v (r:rs) (Prog x) g' =
          let z = applyRule (allVars g' ++ v) r g'
          in if isNothing z then helper v rs (Prog x) g'
             else (getSubst z, Node (getGoal z) (helper (getVarNames z) x (Prog x) (getGoal z))):helper (getVarNames z) rs (Prog x) g'

        -- | Returns the VarNames.
        getVarNames :: Maybe ([VarName], Goal, Subst) -> [VarName]
        getVarNames (Just (v,_,_)) = v
        getVarNames Nothing        = []

        -- | Returns the goal.
        getGoal :: Maybe ([VarName], Goal, Subst) -> Goal
        getGoal (Just (_,t,_))    = t
        getGoal Nothing           = Goal []

        -- | Returns the substitution.
        getSubst :: Maybe ([VarName], Goal, Subst) -> Subst
        getSubst (Just (_,_,s))   = s
        getSubst Nothing          = empty

applyRule :: [VarName] -> Rule -> Goal -> Maybe ([VarName], Goal, Subst)
-- | Applies a rule to the first term of a goal. Changes the VarNames of a rule to prevent name conflicts.
applyRule v r = helper (v ++ allVars (rename v r)) (rename v r)
  -- | Applies a rule to the first term of a goal.
  where helper :: [VarName] -> Rule -> Goal -> Maybe ([VarName], Goal, Subst)
        helper _ _           (Goal [])  = Nothing
        helper y (Rule x xs) (Goal (t:ts))
          | isNothing (unify x t)       = Nothing
          | otherwise                   = let z = removeMaybe (unify x t)
                                          in Just (y, Goal (map (apply z) (xs ++ ts)), z)

        -- Removes the maybe of a substitution.
        removeMaybe :: Maybe Subst -> Subst
        removeMaybe (Just x) = x
        removeMaybe Nothing  = empty

-- | Defines how to get all substitutions from a sld tree.
type Strategy = SLDTree -> [Subst]

-- | Using depth-first search strategy to get all solutions.
dfs :: Strategy
dfs sldTree = dfs' sldTree empty
  -- | Using depth-first search strategy to get all solutions.
  where dfs' :: SLDTree -> Subst -> [Subst]
        dfs' (Node (Goal []) []) s = [s]
        dfs' (Node (Goal _) [])  _ = []
        dfs' (Node _ xs)         s = concatMap (\(a, b) -> dfs' b (compose a s)) xs

-- | Using breadth-first search strategy to get all solutions.
bfs :: Strategy
bfs sldTree = bfs' [(sldTree, empty)]
  -- | Using breadth-first search strategy to get all solutions.
  where bfs' :: [(SLDTree, Subst)] -> [Subst]
        bfs' []                         = []
        bfs' ((Node (Goal []) [],s):as) = s:bfs' as
        bfs' ((Node (Goal _ ) [],_):as) = bfs' as
        bfs' ((Node _         xs,s):as) = bfs' (as ++ helper xs s)

        -- | Extends the substitution.
        helper :: [(Subst, SLDTree)] -> Subst -> [(SLDTree, Subst)]
        helper []         _ = []
        helper ((a,b):as) s = (b,compose a s):helper as s

-- | Solves a given goal and program with a given strategy.
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g s = map (\x -> restrictTo x (allVars g)) (s (sld p g))