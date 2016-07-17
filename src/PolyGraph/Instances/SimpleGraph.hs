{-
  Simple Graph in math is graph without loops and no multiple edges.
  This implementation does not prevent loops but it has this forgetfulness property:
  for di-graph it ignores multiple edges treating
  them as one edge (even if used with a list).

  TODO Needs work/rethinking to achieve the same forgetfullness for non-di graphs.

  This data type treats no-multiple edges requirement on the type level.

  Assumption that graph has no multiple edges is viewed as: edge does not contribute
  more information than 2 vertices it joins.  Hence edge can be simply represented as a pair.
-}
module PolyGraph.Instances.SimpleGraph (
     SimpleGraph(..)
   , SimpleListDiGraph
   , SimpleSetDiGraph
) where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import PolyGraph.Common.BuildableCollection
import Data.List (nub, null, lines, words, concat)
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS
import qualified Data.Foldable as F

data SimpleGraph  v e t   = SimpleGraph { getEdges:: t e, getDisconnectedVertices:: t v}
type SimpleListDiGraph v = SimpleGraph v (OPair v) []
type SimpleSetDiGraph v  = SimpleGraph v (OPair v) HS.HashSet

-- INSTANCES --
-- TODO needs more reusable instance logic, use common.helpers.BuildableCollection
--foldMap :: Monoid m => (a -> m) -> t a -> m
instance forall v e t. (Show v, Foldable t, Show e, PairLike e v) => Show (SimpleGraph v e t) where
  show g =  let looseVerticesS = F.foldMap (\v -> show(v)++",") (getDisconnectedVertices g)
                looseVerticesD = if looseVerticesS == []
                                 then ""
                                 else "Loose Vertices: " ++ looseVerticesS ++ "\n"
                edgesS = F.foldMap (\vv -> " " ++ show(vv) ++ "\n") (getEdges g)
                edgesD = if edgesS == []
                         then "No Edges"
                         else "Edges: \n" ++ edgesS
            in looseVerticesD ++ edgesD

----------------------------------------------
-- ReadOnly Graph INSTANCES SimpleGraph v t --
----------------------------------------------
instance  forall v e t. (Eq v, Foldable t, PairLike e v) => (GraphDataSet (SimpleGraph v e t) v e t) where
  isolatedVertices g = getDisconnectedVertices g
  edges g  =  getEdges g

--
-- TODO
-- this basically forces [] as Index type, can be genralized but will currently cause ambiguities
-- HashSet is not a Traversable so this would not be sufficient for HashSet if the same type was used
--
instance forall v t. (Eq v, Foldable t, BuildableCollection (t (OPair v)) (OPair v)) =>
                                                      (DiAdjacencyIndex (SimpleGraph v (OPair v) t) v (OPair v) []) where
  cEdgesOf g ver =
                let addEdge :: (OPair v) -> t (OPair v) -> t (OPair v)
                    addEdge vv tvv =
                           if (oPairFirst vv == ver)
                           then addBuildableElement vv tvv
                           else tvv
                in F.toList $ foldr addEdge emptyBuildableCollection (getEdges g)

--
-- NOTE:
-- Seems I do an override with faster implementation? like so:
--
instance forall v t. (Eq v) => (DiAdjacencyIndex (SimpleSetDiGraph v) v (OPair v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> oPairFirst vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v t. (Eq v, Foldable t) => (Graph (SimpleGraph v (UOPair v) t) v (UOPair v) t)

instance  forall v t. (Eq v, Foldable t) => (DiGraph (SimpleGraph v (OPair v) t) v (OPair v) t)

------------------------------
-- Buildable Graph instance --
-- no lenses no fun
------------------------------
instance  forall v e t . (Eq v,
                        Foldable t,
                        AdjustableCollection (t e) e,
                        AdjustableCollection (t v) v,
                        PairLike e v
                        ) => BuildableGraphDataSet(SimpleGraph v e t) v e t where

   empty = SimpleGraph (emptyBuildableCollection :: t e) (emptyBuildableCollection :: t v)

   g @+ v = let newVertices = addUniqueBuildableElement v (getDisconnectedVertices g)
            in g {getDisconnectedVertices = newVertices}
   g ~+ e =
            let (v1,v2) = toPair e
                newVertices = deleteBuildableElement v1 $
                              deleteBuildableElement v2 (getDisconnectedVertices g)
                newEdges = addUniqueBuildableElement (fromPair (v1,v2)) (getEdges g)
            in g {getEdges = newEdges}
   union g1 g2 =
            let newEdges = (getEdges g1) `unionBuildableCollections` (getEdges g2)
                newVertices = (getDisconnectedVertices g1) `unionBuildableCollections` (getDisconnectedVertices g2)
            in g1 {getEdges = newEdges, getDisconnectedVertices = newVertices}

--------------------------------------------
-- Adjustable Graph instance              --
--------------------------------------------
instance  forall v e t . (Eq v,
                        Foldable t,
                        AdjustableCollection (t e) e,
                        AdjustableCollection (t v) v,
                        PairLike e v
                        ) => AdjustableGraphDataSet(SimpleGraph v e t) v e t where

   g @\ f = let newVertices = filterBuildableCollection f (getDisconnectedVertices g)
                newEdges = filterBuildableCollection (\vv -> (f $ first vv) && (f $ second vv)) (getEdges g)
            in  SimpleGraph { getEdges = newEdges, getDisconnectedVertices = newVertices}

   filterEdges strict g f = let newEdges = filterBuildableCollection f (getEdges g)
            in if strict
               then g {getEdges = newEdges}
               else g {getEdges = newEdges, getDisconnectedVertices = emptyBuildableCollection}
