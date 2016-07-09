
module Play.DiGraph.PolyBuild where

import Play.DiGraph.Types
import PolyGraph.DiGraph
import PolyGraph.Graph.PolyBuild
import qualified PolyGraph.Helpers as H


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

diamondABCD :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) => g
diamondABCD = diamond "10" "11" "12" "20"

showDiamondABCD1 =
                let mygraph:: SimpleSetGraph String
                    mygraph = diamondABCD
                in  show (mygraph)

showDiamondABCD2 =
                let mygraph :: SimpleSetGraph Int
                    mygraph = diamondABCD
                in show (mygraph)

showDiamondABCD3 =
                let mygraph :: Theory
                    mygraph = diamondABCD
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

diamondABCD' :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => (String -> v) -> g
diamondABCD' f = diamond' (f "10") (f "11") (f "12") (f "20")

showDiamond'ABCD1 =
                let mygraph:: SimpleSetGraph String
                    mygraph = diamondABCD' id
                in  show (mygraph)


showDiamond'ABCD3 =
                let mygraph :: Theory
                    mygraph = diamondABCD' Statement
                in show (mygraph)
