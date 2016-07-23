module PolyGraph.Buildable where

import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics)
import qualified PolyGraph.Common as H

class BuildableEdgeSemantics e v where
  defaultEdge :: v -> v -> e

instance forall v. (Read v) => BuildableEdgeSemantics (H.OPair v) v where
  defaultEdge v1 v2 = H.OPair (v1,v2)

class FromString a where
  fromString :: String -> a


instance {-# OVERLAPPABLE #-} forall v. (Read v) => FromString v where
  fromString = read

instance {-# OVERLAPPING #-} FromString String where
    fromString = id

class GraphDataSet g v e t  => BuildableGraphDataSet g v e t where
  empty :: g

  emptyGraph :: g
  emptyGraph = empty
  -- | adds vertex, implemenation decides what to do if same vertex is added twice
  (@+) :: g -> v -> g

  -- | adds edge (and vertices if needed), implementation decides what to do if same edge is added twice, Eq not assumed on e here
  (~+) :: g -> e -> g

  (?+) :: g -> Either v e -> g
  g ?+ Left v = g @+ v
  g ?+ Right e = g ~+ e

  (@++) :: g -> [v] -> g
  g @++ []     = g
  g @++ (v:vs) = g @+ v @++ vs

  (~++) :: g -> [e] -> g
  g ~++ []     = g
  g ~++ (e:es) = g ~+ e ~++ es

  union :: g -> g -> g

-- adds edge with default semantics between vertices
addDefaultEdge :: forall g v e t . (BuildableEdgeSemantics e v, BuildableGraphDataSet g v e t ) => v -> v -> g -> g
addDefaultEdge v1 v2 g = g ~+ (defaultEdge v1 v2)
