module Play.DiGraph.SampleInstances.SimpleGraph (
     SimpleGraph(..)
   , SimpleListGraph
   , SimpleSetGraph
) where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import Data.List (nub, null, lines, words, concat)
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.HashMapCIndex as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS
import qualified Data.Foldable as F

--
-- Simple implemenation of DiGraph
-- Note: getEdges is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'getEdges sg'
--
data SimpleGraph v t   = SimpleGraph { getEdges:: t (v,v), getDisconnectedVertices:: t v}
type SimpleListGraph v = SimpleGraph v []
type SimpleSetGraph v  = SimpleGraph v HS.HashSet

-- INSTANCES --
-- TODO needs more reusable instance logic, use common.helpers.BuildableDependentCollection
--foldMap :: Monoid m => (a -> m) -> t a -> m
instance forall v t . (Show v, Foldable t) => Show (SimpleGraph v t) where
  show g =  let looseVerticesS = F.foldMap (\v -> show(v)++",") (getDisconnectedVertices g)
                looseVerticesD = if looseVerticesS == []
                                 then ""
                                 else "Loose Vertices: " ++ looseVerticesS ++ "\n"
                edgesS = F.foldMap (\vv -> " " ++ show(vv)++ "\n") (getEdges g)
                edgesD = if edgesS == []
                         then "No Edges"
                         else "Edges: \n" ++ edgesS
            in looseVerticesD ++ edgesD

-- INSTANCES SimpleGraph v [] --
instance  forall v . (Eq v) => (GraphDataSet (SimpleListGraph v) v (v,v) []) where
  isolatedVertices g = getDisconnectedVertices g
  edges g  =  getEdges g

instance forall v t. (Eq v) => (CIndex (SimpleListGraph v) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleListGraph v) v (v,v) [])

-- INSTANCES SimpleGraph v HashSet --
instance  forall v . (HASH.Hashable v, Eq v) => (GraphDataSet (SimpleSetGraph v) v (v,v) HS.HashSet) where
  isolatedVertices g = getDisconnectedVertices g
  edges g  =  getEdges g

instance forall v t. (Eq v) => (CIndex (SimpleSetGraph v) v (v,v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleSetGraph v) v (v,v) HS.HashSet)

-- no lenses no fun
instance  forall v . (HASH.Hashable v, Eq v) => (BuildableGraphDataSet(SimpleSetGraph v) v (v,v) HS.HashSet) where
   empty = SimpleGraph HS.empty HS.empty

   --TODO fix this issue: adds vertex even if it already exists in the graph
   g @+ v = let newVertices = HS.insert v (getDisconnectedVertices g)
            in g {getDisconnectedVertices = newVertices}
   g ~+ (v1,v2) =
            let newVertices = HS.delete v1 $ HS.delete v2 (getDisconnectedVertices g)
                newEdges = HS.insert (v1,v2) (getEdges g)
            in g {getEdges = newEdges}
   union g1 g2 =
            let newEdges = (getEdges g1) `HS.union` (getEdges g2)
                newVertices = (getDisconnectedVertices g1) `HS.union` (getDisconnectedVertices g2)
            in g1 {getEdges = newEdges, getDisconnectedVertices = newVertices}

instance forall v . (HASH.Hashable v, Eq v) => (AdjustableGraphDataSet (SimpleGraph v HS.HashSet) v (v,v) HS.HashSet) where
   g @\ f = let newVertices = HS.filter f (getDisconnectedVertices g)
                newEdges = HS.filter (\vv -> (f $ first' vv) && (f $ second' vv)) (getEdges g)
            in  SimpleGraph { getEdges = newEdges, getDisconnectedVertices = newVertices}

   filterEdges strict g f = let newEdges = HS.filter f (getEdges g)
            in if strict
               then g {getEdges = newEdges}
               else g {getEdges = newEdges, getDisconnectedVertices = HS.empty}
