{-
 Polymorphic production with specialized consumption example.
-}
module Play.DiGraph.PolyBuild where

import qualified Play.DiGraph.SampleInstances.FirstLastWord as FL
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as HM
import qualified PolyGraph.Instances.AdjacencyMatrix as AM
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Buildable.DiGBuild
import qualified PolyGraph.Common as H


-------
-- convenient, but bad, code prevents typechecked to do its job
-------
diamond :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => String -> String -> String -> String -> g
diamond s1 s11 s12 s2 =  ( s1 ^+~>^ s2  ) .
                         ( s1 ^+~>^ s11 ) .
                         ( s1 ^+~>^ s12 ) .
                         ( s11 ^+~>^ s2 ) .
                         ( s12 ^+~>^ s2 ) $ emptyGraph

diamond0123 :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) => g
diamond0123 = diamond "0" "1" "2" "3"

showDiamond0123_1 = show (diamond0123:: SG.SimpleSetDiGraph String)

showDiamond0123_2 = show (diamond0123 :: SG.SimpleSetDiGraph Int)

showDiamond0123_3 = show (diamond0123 :: FL.FLWordText)

showDiamond0123_4 = show (diamond0123 :: HM.DiEdgesByVertexMap String (H.OPair String) [])

showDiamond0123_5 = AM.prettyAdjacencyMatrix (diamond0123 :: AM.DiAdjacencyMatrix Int)

-----------------------------------------
-- better code, locks on type I want   ---
-----------------------------------------
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

-------------------------------------------------------
--- Back to diamond0123                              ---
-------------------------------------------------------


-- TODO continue
