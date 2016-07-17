{-
  This graph data structure stores full topological graph information remembering the number of edges
  between give pair of vertices, can be used both as Graph and DiGraph with correct instances.
  It is essential in implementation of polymorphic Eq for graphs / digraphs (TODO)
-}

module PolyGraph.Instances.EdgeCountMapGraph where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as S


--
-- HashMap (v0,v0) 0 represents any vertex without loop
-- HashMap (v0,v0) 1 represents loop
-- HashMap (v0,v1) 2 represents 2 edges from v0 to v1
--
-- Default Eq implementation defines DiGraph equality for HPair and Graph equality
-- for UnorderedHPair.  2 graphs are considered equal if for each of the verices they
-- have the same number of adjacent edges
--
data EdgeCountMap e = EdgeCountMap {
    getMap :: HM.HashMap e Int
} deriving (Show, Eq)

type DiGraphEdgeCountMap v = EdgeCountMap (HPair v)
type GraphEdgeCountMap v   = EdgeCountMap (UnorderedHPair v)

-- INSTANCES --
instance  forall v e. (Eq v, Hashable v, Eq e, Hashable e, HPairLike e v) =>
                              (GraphDataSet (EdgeCountMap e) v e S.Seq) where

  --filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k vSource
  isolatedVertices g =
                    let zeroCountFilter :: e -> Int -> Bool
                        zeroCountFilter edge count = (count == 0) && oneElPair edge
                        allverticesWithoutLoops :: HS.HashSet v
                        allverticesWithoutLoops = HS.fromList . map (first' . toPair) . HM.keys . HM.filterWithKey zeroCountFilter . getMap $ g
                        -- HM.foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
                        isolatedVertices = undefined :: HS.HashSet v
                    in S.fromList . HS.toList $ isolatedVertices

  --foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
  -- | edges replay same HPair several times for multiedge graphs
  edges g  =
              let  foldF :: e -> Int -> S.Seq e -> S.Seq e
                   foldF edge count res =
                                  (S.replicate count edge) S.>< res
              in HM.foldrWithKey foldF S.empty (getMap g)

--
-- No DiAdjacencyIndex instance, probably not needed
--
instance  forall v. (Eq v, Hashable v) =>
                                 (DiGraph (EdgeCountMap (HPair v)) v (HPair v) S.Seq)

instance  forall v. (Eq v, Hashable v) =>
                                 (Graph (EdgeCountMap (UnorderedHPair v)) v (UnorderedHPair v) S.Seq)

--
instance  forall v e. (Eq v, Hashable v, Eq e, Hashable e, HPairLike e v) =>
                              (BuildableGraphDataSet (EdgeCountMap e) v e S.Seq) where

   empty = EdgeCountMap HM.empty

   -- insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
   -- insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v -> HashMap k v
   g @+ v = undefined -- needs insertWith to keep previous if loops

   g ~+ e = undefined

   union g1 g2 = undefined
