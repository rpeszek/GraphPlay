module Play.DiGraph.PolyRebuild where

import PolyGraph.Buildable.GDSBuild
import PolyGraph.Buildable.PolyRebuild
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.HashMapAsDiGraph as HM

-- create simple graph data structure
edgeList :: [(Int, Int)]
edgeList = [(0,1), (0,2), (0,3), (1,3), (2,3)]

diamond0123Simple :: SG.SimpleListGraph Int
diamond0123Simple = SG.SimpleGraph edgeList []

-- simple poly rebuilding into polymorphic graph
diamond0123 :: forall g t . (BuildableGraphDataSet g Int (Int, Int) t)
                           =>  g
diamond0123 = polyRebuild diamond0123Simple

-- use it as something else
showDiamond0123AsHashMap = let mygraph :: HM.DiGraphHashMap Int (Int,Int) []
                               mygraph = diamond0123
                           in  show (mygraph)

-- TODO poly transformation
