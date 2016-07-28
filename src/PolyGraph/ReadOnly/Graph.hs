--
--
module PolyGraph.ReadOnly.Graph (
   EdgeSemantics(..)
   , Graph (..)
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
