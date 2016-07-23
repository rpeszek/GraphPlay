module Deprecated.DiGraph.PolyRebuild where

import PolyGraph.Common
import PolyGraph.Buildable
import PolyGraph.Buildable.PolyRebuild
import PolyGraph.Buildable.PolyMorth
import PolyGraph.ReadOnly.DiGraph.DiGraphEquality
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as HM
import Data.Hashable

-- create simple graph data structure
edgeList :: [(Int, Int)]
edgeList = [(0,1), (0,2), (0,3), (1,3), (2,3)]

diamond0123Simple :: SG.SimpleListDiGraph Int
diamond0123Simple = SG.SimpleGraph (map (OPair) edgeList) []

-- Polymorphic production:
-- simple poly rebuilding into polymorphic graph
diamond0123 :: forall g t . (BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond0123 = polyRebuild diamond0123Simple

areEqual :: Bool
areEqual = diamond0123Simple ~>#== diamond0123     -- returns True

diamond3456 :: forall g t . (BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond3456 = fmorth (+3) diamond0123Simple

diamondChain :: forall g t . (BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamondChain =  foldr (\i g -> g `union` (fmorth (+i) diamond0123Simple)) empty (map (*3) [0..5])

areNotEqual = diamond0123Simple ~>#== diamondChain -- returns False

-- specialized type consumption

showDiamond0123AsHashMap  = show (diamond0123  :: HM.DiEdgesByVertexMap Int (OPair Int) [])
showDiamond3456AsHashMap  = show (diamond3456  :: HM.DiEdgesByVertexMap Int (OPair Int) [])
showDiamondChainAsHashMap = show (diamondChain :: HM.DiEdgesByVertexMap Int (OPair Int) [])
