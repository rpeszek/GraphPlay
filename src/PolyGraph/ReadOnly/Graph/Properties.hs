-- Properties for Graph
--
module PolyGraph.ReadOnly.Graph.Properties (
   isValidGraph
   , isValidMorphism
   , isValidMorphismSingleEdge
) where

import PolyGraph.ReadOnly (GMorphism(..), isValidGraphDataSet)
import PolyGraph.ReadOnly.Graph (Graph, EdgeSemantics(..))
import PolyGraph.Common (UOPair(..), PairLike(toPair))
import qualified Data.Maybe as M
import qualified Data.Foldable as F

-- This code could be reused better between Graph and DiGraph
isValidGraph :: forall g v e t . Graph g v e t => g -> Bool
isValidGraph = isValidGraphDataSet (toPair . resolveEdge)

-- | to be valid eTrans and resolveEdge needs to commute with the vTrans ignoring the pair order
isValidMorphism :: forall g v0 e0 t v1 e1 . (Eq v0, Eq v1, EdgeSemantics e0 v0, EdgeSemantics e1 v1) =>
                               [e0] -> GMorphism v0 e0 v1 e1 -> Bool
isValidMorphism es m = M.isNothing $ F.find (isValidMorphismSingleEdge m) es


-- | NOTE UOPair == is diffrent from OPair ==
-- forcing different implementation for EdgeSemantics and DiEdgeSemantics
isValidMorphismSingleEdge :: forall g v0 e0 t v1 e1 . (Eq v0, Eq v1, EdgeSemantics e0 v0, EdgeSemantics e1 v1) =>
                                  GMorphism v0 e0 v1 e1 -> e0 -> Bool
isValidMorphismSingleEdge m e0 =
                       let UOPair(v0a, v0b) = resolveEdge e0
                           e1 = eTrans m e0
                       in  UOPair(vTrans m v0a, vTrans m v0b) == resolveEdge e1
