--
--
module PolyGraph.ReadOnly.Graph (
   EdgeSemantics(..)
   , Graph
   , AdjacencyIndex(..)
) where --exports everything, a terrible programmer wrote it

import PolyGraph.ReadOnly (GraphDataSet)
import PolyGraph.Common (UOPair)

--
-- e are edges v are vertices, the order of (OPair v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v  where
  resolveEdge      ::  e -> UOPair v

instance forall v . (Eq v) => (EdgeSemantics (UOPair v) v) where
  resolveEdge e = e

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t

class (Traversable t, EdgeSemantics e v)  => AdjacencyIndex g v e t | g -> t, g -> v, g -> e where
  edgesOf   ::  g -> v -> t e   -- return a list of adjacent edges, empty if not a valid vertex or isolated vertex
