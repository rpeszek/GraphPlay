
module PolyGraph.ReadOnly.DiGraph.Properties where

import PolyGraph.ReadOnly (GMorphism(..), isValidGraphDataSet)
import PolyGraph.ReadOnly.DiGraph (DiGraph, DiEdgeSemantics(..))
import PolyGraph.Common (OPair(..), PairLike(toPair))
import qualified Data.Maybe as M
import qualified Data.Foldable as F

isValidDiGraph :: forall g v e t . DiGraph g v e t => g -> Bool
isValidDiGraph  = isValidGraphDataSet (toPair . resolveDiEdge)


-- | to be valid eTrans and resolveEdge needs to commute with the vTrans ignoring the pair order
isValidMorphism :: forall v0 e0  v1 e1 . (Eq v0, Eq v1, DiEdgeSemantics e0 v0, DiEdgeSemantics e1 v1) =>
                               [e0] -> GMorphism v0 e0 v1 e1 -> Bool
isValidMorphism es m = M.isNothing $ F.find (isValidMorphismSingleEdge m) es


-- | NOTE UOPair == is diffrent from OPair ==
-- forcing different implementation for EdgeSemantics and DiEdgeSemantics
isValidMorphismSingleEdge :: forall v0 e0 v1 e1 . (Eq v0, Eq v1, DiEdgeSemantics e0 v0, DiEdgeSemantics e1 v1) =>
                                  GMorphism v0 e0 v1 e1 -> e0 -> Bool
isValidMorphismSingleEdge m e0 =
                       let OPair(v0a, v0b) = resolveDiEdge e0
                           e1 = eTrans m e0
                       in  OPair(vTrans m v0a, vTrans m v0b) == resolveDiEdge e1
