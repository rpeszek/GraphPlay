
module PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge (
   EdgeHelper(..)
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

data EdgeHelper e v = EdgeHelper {
    getEdge      :: e,
    getVertices   :: (v,v)
} deriving Show

instance forall e v.(Eq v) => Eq(EdgeHelper e v) where
  fedge1 == fedge2 = (getVertices fedge1) == (getVertices fedge2)


instance forall v e map. DiEdgeSemantics (EdgeHelper e v) v where
   resolveDiEdge indexedE = getVertices indexedE

instance forall v e map. EdgeSemantics (EdgeHelper e v) v where
   resolveEdge indexedE = getVertices indexedE

convertToEdgeHelper :: forall e v . (e -> (v,v)) -> e -> EdgeHelper e v
convertToEdgeHelper f e = EdgeHelper { getEdge = e, getVertices = f e }