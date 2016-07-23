{-
  This graph data structure stores full topological graph information remembering the number of edges
  between give pair of vertices, can be used both as Graph and DiGraph with correct instances.
  It is essential in implementation of polymorphic Eq for graphs / digraphs (TODO)
-}

module PolyGraph.Instances.EdgeCountMapGraph (
    EdgeCountMapGraph (..)
    , EdgeCountMapDiGraph (..)
    , EdgeCountMap (..)
) where

import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable
import PolyGraph.Adjustable
import PolyGraph.Common
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Data.Matrix as M


--
-- vertices are stored separately in a set
-- HashMap (v0,v0) 1 represents loop
-- HashMap (v0,v1) 2 represents 2 edges from v0 to v1
--
-- Default Eq implementation defines DiGraph equality for OPair and Graph equality
-- for UOPair.  2 graphs are considered equal if for each of the verices they
-- have the same number of adjacent edges
--
data EdgeCountMap v e = EdgeCountMap {
    getMap :: HM.HashMap e Int,
    getVertices :: HS.HashSet v
} deriving (Show, Eq)

type EdgeCountMapDiGraph v = EdgeCountMap v (OPair v)
type EdgeCountMapGraph v   = EdgeCountMap v (UOPair v)

-- INSTANCES --
instance  forall v e. (Eq v, Hashable v, Eq e, Hashable e, PairLike e v) =>
                              (GraphDataSet (EdgeCountMap v e) v e S.Seq) where

  --HM.foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
  isolatedVertices g =
                    let foldF :: e -> Int -> HS.HashSet v -> HS.HashSet v
                        foldF _ 0 vertices = vertices
                        foldF edge count vertices =
                                    let (v1,v2) = toPair edge
                                    in foldr HS.delete vertices [v1,v2]
                        isolatedVertices = HM.foldrWithKey foldF (getVertices g) (getMap g)
                    in S.fromList . HS.toList $ isolatedVertices

  -- | edges replay same OPair several times for multiedge graphs
  edges g  =
              let  foldF :: e -> Int -> S.Seq e -> S.Seq e
                   foldF edge count res =
                                  (S.replicate count edge) S.>< res
              in HM.foldrWithKey foldF S.empty (getMap g)

--
-- No DiAdjacencyIndex instance, probably not needed
--
instance  forall v. (Eq v, Hashable v) =>
                                 (DiGraph (EdgeCountMap v (OPair v)) v (OPair v) S.Seq)

instance  forall v. (Eq v, Hashable v) =>
                                 (Graph (EdgeCountMap v (UOPair v)) v (UOPair v) S.Seq)

--
instance  forall v e. (Eq v, Hashable v, Eq e, Hashable e, PairLike e v) =>
                              (BuildableGraphDataSet (EdgeCountMap v e) v e S.Seq) where

   empty = EdgeCountMap HM.empty HS.empty

   -- insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
   -- insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
   g @+ v =
           let newVertices = HS.insert v (getVertices g)
           in g {getVertices = newVertices}

   g ~+ e =
           let (v1,v2) = toPair e
               newVertices = foldr HS.insert (getVertices g) [v1,v2]
               newMap = HM.insertWith (\oldCount _ -> oldCount + 1) e (1::Int) (getMap g)
           in EdgeCountMap {getMap = newMap, getVertices = newVertices}

   -- unionWith :: (Eq k, Hashable k) => (v -> v -> v) -> HashMap k v -> HashMap k v -> HashMap k v
   union g1 g2 =
           let newVertices = HS.union (getVertices g1) (getVertices g2)
               newMap = HM.unionWith(+) (getMap g1) (getMap g2)
           in EdgeCountMap {getMap = newMap, getVertices = newVertices}
