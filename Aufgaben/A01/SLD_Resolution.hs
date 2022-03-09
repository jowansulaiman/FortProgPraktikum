{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : SLD_Resolution
Description : SLD trees and strategies for traversing them.
Maintainer  : Jowan Sulaiman and Kjell Rothenburger

The module contains the following functions $'sld', $'apply_rule', $'dfs',
$'bfs' and $'solveWith'.
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

data SLDTree = Node Goal [(Subst,SLDTree)]
-- | Data type 'SLD trees'
  deriving Show

-- Constructs the SLDTree by resolving the Goal using the Prog.
sld :: Prog -> Goal -> SLDTree
-- Reached empty clause (leaf)
sld _  (Goal [])        = Node (Goal []) []
-- Rename every rule and try to apply each rule to the Goal terms
sld (Prog rs1) (Goal ts) = let rs2 = map (rename (allVars (Goal ts))) rs1
                            in Node (Goal ts) (concatMap (sld_helper rs2 ts) rs2)

-- Tries to apply a single rule to the leftmost term
apply_rule :: [Rule] -> [Term] -> Rule -> [(Subst,SLDTree)]
--  |         all rules; terms to prove; current rule
apply_rule r ars (t:ts) (Rule tl tr) = case (unify tl t) of
                                       Nothing  -> []
                                       Just mgu -> [(mgu,sld (Prog ars) (Goal (map (apply mgu) (tr ++ ts))))]

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
