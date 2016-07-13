
module PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedDiEdge (
   DiEdgeHelper(..)
) where

import qualified Data.Foldable as F
import qualified Data.Maybe as MB
import qualified Data.HashMap.Strict as HM
import PolyGraph.Common.Helpers
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.ReadOnly.Graph

------------------------------------------------------------------------------------------
--  DiEdgeSemantics indexer should be needed only if rerieval of v-s from edges is slow --
-- for example is e -> (v,v) needs to parse a text to lookup vertices                   --
------------------------------------------------------------------------------------------

data DiEdgeHelper e v = DEdgeWithIndexedSemantics {
    getDEdge      :: e,
    getVertices   :: (v,v)
}

instance forall e v.(Eq v) => Eq(DiEdgeHelper e v) where
  fedge1 == fedge2 = (getVertices fedge1) == (getVertices fedge2)


instance forall v e map. DiEdgeSemantics (DiEdgeHelper e v) v where
   resolveDiEdge indexedE = getVertices indexedE
