
module PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge (
   EdgeHelper(..)
  , convertToEdgeHelper
) where

import PolyGraph.Common (OPair)
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics, DiEdgeSemantics(..))

------------------------------------------------------------------------------------------
--  DiEdgeSemantics indexer should be needed only if rerieval of v-s from edges is slow --
-- for example is e -> (OPair v) needs to parse a text to lookup vertices                   --
------------------------------------------------------------------------------------------

data EdgeHelper e v = EdgeHelper {
    getEdge      :: e,
    getVertices   :: OPair v
} deriving Show

instance forall e v.(Eq v) => Eq(EdgeHelper e v) where
  fedge1 == fedge2 = (getVertices fedge1) == (getVertices fedge2)


instance forall v e . DiEdgeSemantics (EdgeHelper e v) v where
   resolveDiEdge indexedE = getVertices indexedE

convertToEdgeHelper :: forall e v . (e -> (OPair v)) -> e -> EdgeHelper e v
convertToEdgeHelper f e = EdgeHelper { getEdge = e, getVertices = f e }
