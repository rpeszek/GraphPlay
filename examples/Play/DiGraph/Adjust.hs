
module Play.DiGraph.Adjust where

import Play.DiGraph.Types
import PolyGraph.DiGraph
import PolyGraph.Graph.Adjust
import qualified PolyGraph.Helpers as H


-------
-- convenient but bad code, prevents typechecked to do its job
-------
diamond :: forall g v e t . (H.FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => String -> String -> String -> String -> g
diamond s1 s11 s12 s2 =  ( s1 ^+~>^ s2 ) .
                         ( s1 ^+~>^ s11) .
                         ( s1 ^+~>^ s12) .
                         ( s11 ^+~>^ s2) .
                         ( s12 ^+~>^ s2 ) $ emptyGraph

diamondABCD :: forall g v e t . (H.FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) => g
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
--displayABCD2 = show(diamondABCD :: Theory)
