{-
  Simple Graph in math is graph without loops and no multiple edges.
  This implementation does not prevent loops and prevenents multiple edges only if used
  with a Set collection (SimpleSetGraph v).

  This data type treats no-multiple edges requirement on the type level.

  Assumption that graph has no multiple edges is viewed as: edge does not contribute
  more information than 2 vertices it joins.  Hence edge can be simply represented as a pair.
-}
module PolyGraph.Instances.SimpleGraph (
     SimpleGraph(..)
   , SimpleListGraph
   , SimpleSetGraph
) where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import PolyGraph.Common.InstanceHelpers
import Data.List (nub, null, lines, words, concat)
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS
import qualified Data.Foldable as F

data SimpleGraph v t   = SimpleGraph { getEdges:: t (v,v), getDisconnectedVertices:: t v}
type SimpleListGraph v = SimpleGraph v []
type SimpleSetGraph v  = SimpleGraph v HS.HashSet

-- INSTANCES --
-- TODO needs more reusable instance logic, use common.helpers.BuildableDependentCollection
--foldMap :: Monoid m => (a -> m) -> t a -> m
instance forall v t. (Show v, Foldable t) => Show (SimpleGraph v t) where
  show g =  let looseVerticesS = F.foldMap (\v -> show(v)++",") (getDisconnectedVertices g)
                looseVerticesD = if looseVerticesS == []
                                 then ""
                                 else "Loose Vertices: " ++ looseVerticesS ++ "\n"
                edgesS = F.foldMap (\vv -> " " ++ show(vv)++ "\n") (getEdges g)
                edgesD = if edgesS == []
                         then "No Edges"
                         else "Edges: \n" ++ edgesS
            in looseVerticesD ++ edgesD

----------------------------------------------
-- ReadOnly Graph INSTANCES SimpleGraph v t --
----------------------------------------------
instance  forall v t. (Eq v, Foldable t) => (GraphDataSet (SimpleGraph v t) v (v,v) t) where
  isolatedVertices g = getDisconnectedVertices g
  edges g  =  getEdges g

--
-- this basically forces [] as Index type, can be genralized but will currently cause ambiguities
-- HashSet is not a Traversable so this would not be sufficient for HashSet if the same type was used
--
instance forall v t. (Eq v, Foldable t, BuildableDependentCollection (t (v,v)) (v,v)) =>
                                                      (DiAdjacencyIndex (SimpleGraph v t) v (v,v) []) where
  cEdgesOf g ver =
                let addEdge :: (v,v) -> t (v,v) -> t (v,v)
                    addEdge vv tvv =
                           if (first' vv == ver)
                           then addBuildableElement vv tvv
                           else tvv
                in F.toList $ foldr addEdge emptyBuildableCollection (getEdges g)

--
-- NOTE:
-- Seems I do an override with faster implementation? like so:
--
instance forall v t. (Eq v) => (DiAdjacencyIndex (SimpleSetGraph v) v (v,v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v t. (Eq v, Foldable t) => (DiGraph (SimpleGraph v t) v (v,v) t)

instance  forall v t. (Eq v, Foldable t) => (Graph (SimpleGraph v t) v (v,v) t)

------------------------------
-- Buildable Graph instance --
-- no lenses no fun
------------------------------

instance  forall v t . (Eq v,
                        Foldable t,
                        AdjustableDependentCollection (t (v,v)) (v,v),
                        AdjustableDependentCollection (t v) v
                        ) => BuildableGraphDataSet(SimpleGraph v t) v (v,v) t where

   empty = SimpleGraph (emptyBuildableCollection :: t (v,v)) (emptyBuildableCollection :: t v)

   g @+ v = let newVertices = addUniqueBuildableElement v (getDisconnectedVertices g)
            in g {getDisconnectedVertices = newVertices}
   g ~+ (v1,v2) =
            let newVertices = deleteBuildableElement v1 $
                              deleteBuildableElement v2 (getDisconnectedVertices g)
                newEdges = addUniqueBuildableElement (v1,v2) (getEdges g)
            in g {getEdges = newEdges}
   union g1 g2 =
            let newEdges = (getEdges g1) `unionBuildableCollections` (getEdges g2)
                newVertices = (getDisconnectedVertices g1) `unionBuildableCollections` (getDisconnectedVertices g2)
            in g1 {getEdges = newEdges, getDisconnectedVertices = newVertices}

--------------------------------------------
-- Adjustable Graph instance              --
--------------------------------------------
instance  forall v t . (Eq v,
                        Foldable t,
                        AdjustableDependentCollection (t (v,v)) (v,v),
                        AdjustableDependentCollection (t v) v
                        ) => AdjustableGraphDataSet(SimpleGraph v t) v (v,v) t where

   g @\ f = let newVertices = filterBuildableCollection f (getDisconnectedVertices g)
                newEdges = filterBuildableCollection (\vv -> (f $ first' vv) && (f $ second' vv)) (getEdges g)
            in  SimpleGraph { getEdges = newEdges, getDisconnectedVertices = newVertices}

   filterEdges strict g f = let newEdges = filterBuildableCollection f (getEdges g)
            in if strict
               then g {getEdges = newEdges}
               else g {getEdges = newEdges, getDisconnectedVertices = emptyBuildableCollection}
