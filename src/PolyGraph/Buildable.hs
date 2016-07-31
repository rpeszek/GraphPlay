--
--
module PolyGraph.Buildable (
   BuildableEdgeSemantics(..)
   , BuildableGraphDataSet(..)
   , PrettyRead(..)
   , addDefaultEdge
   , fromEdgeList
   , fromVertexList
   , toSimpleGraphHelper
) where

import qualified PolyGraph.ReadOnly as Base
import qualified PolyGraph.Common as Common
import Data.List (nub)
import Data.Foldable (toList)

class BuildableEdgeSemantics e v where
  defaultEdge :: v -> v -> e

instance forall v. (Read v) => BuildableEdgeSemantics (Common.OPair v) v where
  defaultEdge v1 v2 = Common.OPair (v1,v2)

instance forall v. (Read v) => BuildableEdgeSemantics (Common.UOPair v) v where
  defaultEdge v1 v2 = Common.UOPair (v1,v2)

-- Helper class used for deserializing Vertices when creating graphs --
class PrettyRead a where
  fromString :: String -> a
instance {-# OVERLAPPABLE #-} forall v. (Read v) => PrettyRead v where
  fromString = read
instance {-# OVERLAPPING #-} PrettyRead String where
    fromString = id

class Base.GraphDataSet g v e t  => BuildableGraphDataSet g v e t where
  empty :: g

  emptyGraph :: g
  emptyGraph = empty
  -- | adds vertex, implemenation decides what to do if same vertex is added twice
  (+@) :: g -> v -> g

  -- | adds edge (and vertices if needed), implementation decides what to do if same edge is added twice, Eq not assumed on e here
  (+~) :: g -> e -> g

  (@+) :: v -> g-> g
  (@+) = flip (+@)

  (~+) :: e -> g -> g
  (~+) = flip (+~)

  (?+) :: g -> Either v e -> g
  g ?+ Left v = g +@ v
  g ?+ Right e = g +~ e

  (++@) :: g -> [v] -> g
  g ++@ []     = g
  g ++@ (v:vs) = g +@ v ++@ vs

  (++~) :: g -> [e] -> g
  g ++~ []     = g
  g ++~ (e:es) = g +~ e ++~ es

  union :: g -> g -> g

-- adds edge with default semantics between vertices
addDefaultEdge :: forall g v e t . (BuildableEdgeSemantics e v, BuildableGraphDataSet g v e t ) => v -> v -> g -> g
addDefaultEdge v1 v2 g = g +~ (defaultEdge v1 v2)

fromEdgeList :: forall g v e t . (BuildableGraphDataSet g v e t ) => [e] -> g
fromEdgeList [] = emptyGraph
fromEdgeList (e:xs) = e ~+ fromEdgeList xs

fromVertexList :: forall g v e t . (BuildableGraphDataSet g v e t ) => [v] -> g
fromVertexList [] = emptyGraph
fromVertexList (v:xs) = v @+ fromVertexList xs

toSimpleGraphHelper :: forall g v e t . (Eq e, BuildableGraphDataSet g v e t ) => (e -> (v,v)) -> g -> g
toSimpleGraphHelper edgeRes g = 
                let  notALoop :: e -> Bool
                     notALoop e = let (v1,v2) = edgeRes e in v1 /= v2
                     withoutIsolatedVerts = fromEdgeList . (filter notALoop) . nub . toList . Base.edges $ g
                in withoutIsolatedVerts ++@ (toList . Base.isolatedVertices $ g)
