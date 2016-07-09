
module Play.DiGraph.Adjust where

import Play.DiGraph.Types
import PolyGraph.DiGraph
import PolyGraph.Graph.Adjust


diamond :: forall g v e t . (AdjustableGraphDataSet g v e t, DiEdgeSemantics e v, BuildableEdgeSemantics e v) => v -> v -> v -> v -> g
diamond v1 v2 v3 v4 = (((emptyGraph @@+ v1 $ v2) @@+ v2 $ v4) @@+ v1 $ v3) @@+ v3 $ v4

diamondABCD :: forall g e t . (AdjustableGraphDataSet g String e t, DiEdgeSemantics e String, BuildableEdgeSemantics e String) => g
diamondABCD = diamond "a" "b" "c" "d"

displayABCD1 = show(diamondABCD :: SimpleSetGraph String)
--displayABCD2 = show(diamondABCD :: Theory)
