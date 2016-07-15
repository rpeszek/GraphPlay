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

-- specialized consumption
-- use it as something else


-- TODO does not work, why?
{-
toHashMapHelper :: forall g t . (BuildableGraphDataSet g Int (Int, Int) t)
                                => g -> HM.DiGraphHashMap Int (Int,Int) []
toHashMapHelper gr =   gr

-- NOTE this does not make logical sense!
f :: forall a . (Num a) => a -> Int
f a = a

-- but this does, x is 'forall' in above f acts on arbitry Num:
x :: (Num a) => a
x = 1
y:: Int
y = x
-}

--
showDiamond0123AsHashMap = let mygraph :: HM.DiGraphHashMap Int (Int,Int) []
                               mygraph = diamond0123
                           in  show (mygraph)

--showDiamond0123AsHashMap = showAsHashMapHelper diamond0123


showDiamond3456AsHashMap = let mygraph :: HM.DiGraphHashMap Int (Int,Int) []
                               mygraph = diamond3456
                           in  show (mygraph)


showDiamondChainAsHashMap  = let mygraph :: HM.DiGraphHashMap Int (Int,Int) []
                                 mygraph = diamondChain
                              in  show (mygraph)
