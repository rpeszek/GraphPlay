{-
 Polymorphic production with specialized consumption example.
-}
module Build.E02_SafeDiamond where

import qualified SampleInstances.FirstLastWord as FL
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as HM
import qualified PolyGraph.Instances.AdjacencyMatrix as AM
import qualified PolyGraph.Instances.ListGraphs as LG
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable
import PolyGraph.Buildable.DiGraph
import qualified PolyGraph.Common as H

---
diamond' :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => v -> v -> v -> v -> g
diamond' v1 v11 v12 v2 =  ( v1  @+~>@ v2 ) .
                          ( v1  @+~>@ v11) .
                          ( v1  @+~>@ v12) .
                          ( v11 @+~>@ v2 ) .
                          ( v12 @+~>@ v2 ) $ emptyGraph

diamond0123' :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => (String -> v) -> g
diamond0123' f = diamond' (f "0") (f "1") (f "2") (f "3")

showDiamond'0123_1 =
                let mygraph:: SG.SimpleSetDiGraph String
                    mygraph = diamond0123' id
                in  show (mygraph)


showDiamond'0123_3 =
                let mygraph :: FL.FLWordText
                    mygraph = diamond0123' FL.FLWord
                in show (mygraph)
