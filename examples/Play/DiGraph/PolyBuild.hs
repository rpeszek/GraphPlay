
module Play.DiGraph.PolyBuild where

import qualified Play.DiGraph.SampleInstances.FirstLastWord as FL
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.HashMapAsDiGraph as HM
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Buildable.DiGBuild
import qualified PolyGraph.Common.Helpers as H


-------
-- convenient but bad code, prevents typechecked to do its job
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

showDiamond0123_1 =
                let mygraph:: SG.SimpleSetGraph String
                    mygraph = diamond0123
                in  show (mygraph)

showDiamond0123_2 =
                let mygraph :: SG.SimpleSetGraph Int
                    mygraph = diamond0123
                in show (mygraph)

showDiamond0123_3 =
                let mygraph :: FL.FLWordText
                    mygraph = diamond0123
                in show (mygraph)

showDiamond0123_4 =
                let mygraph :: HM.DiGraphHashMap String (String,String) []
                    mygraph = diamond0123
                in show (mygraph)

-----------------------------------------
-- better code, lock on type I want   ---
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
                let mygraph:: SG.SimpleSetGraph String
                    mygraph = diamond0123' id
                in  show (mygraph)


showDiamond'0123_3 =
                let mygraph :: FL.FLWordText
                    mygraph = diamond0123' FL.FLWord
                in show (mygraph)

-------------------------------------------------------
--- Back to diamond0123                              ---
-------------------------------------------------------
