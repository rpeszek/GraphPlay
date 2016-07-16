module Play.DiGraph.PolyRebuild where

import PolyGraph.Buildable.GDSBuild
import PolyGraph.Buildable.PolyRebuild
import PolyGraph.Buildable.PolyMorth
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.HashMapAsDiGraph as HM
import Data.Hashable

-- create simple graph data structure
edgeList :: [(Int, Int)]
edgeList = [(0,1), (0,2), (0,3), (1,3), (2,3)]

diamond0123Simple :: SG.SimpleListGraph Int
diamond0123Simple = SG.SimpleGraph edgeList []

-- Polymorphic production:
-- simple poly rebuilding into polymorphic graph
diamond0123 :: forall g t . (BuildableGraphDataSet g Int (Int, Int) t) =>  g
diamond0123 = polyRebuild diamond0123Simple

diamond3456 :: forall g t . (BuildableGraphDataSet g Int (Int, Int) t) =>  g
diamond3456 = pmorth (+3) diamond0123Simple

diamondChain :: forall g t . (BuildableGraphDataSet g Int (Int, Int) t) =>  g
diamondChain =  foldr (\i g -> g `union` (pmorth (+i) diamond0123Simple)) empty (map (*3) [0..5])

-- specialized type consumption

showDiamond0123AsHashMap  = show (diamond0123  :: HM.DiGraphHashMap Int (Int,Int) [])
showDiamond3456AsHashMap  = show (diamond3456  :: HM.DiGraphHashMap Int (Int,Int) [])
showDiamondChainAsHashMap = show (diamondChain :: HM.DiGraphHashMap Int (Int,Int) [])
