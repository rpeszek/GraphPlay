module Play.DGraph.TreeMonoidFoldOnSimpleGraph where

----------------------------------------------------------------------
-- test experiments

import PolyGraph.DGraph
import PolyGraph.Helpers
import PolyGraph.DGraph.TreeMonoidFold
import Control.Monad (join)
import Data.List (nub)
import Play.DGraph.Types (SimpleGraph)
import Play.DGraph.Samples (playTwoDimonds)


-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a } deriving Show

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
--
countTreeEdges ::  MonoidFoldAccLogic v e (Sum Int)
countTreeEdges = defaultMonoidFoldAccLogic {
       applyEdge   = const (Sum 1)
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = getSum $ (dfsFold playTwoDimonds (countTreeEdges :: MonoidFoldAccLogic v (v, v) (Sum Int)) "a0") -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

--
-- THIS LIST will have duplicates
--
listChildVertices :: forall v e . (Eq v) => MonoidFoldAccLogic v e [v]
listChildVertices = defaultMonoidFoldAccLogic {
       applyVertex = (\v -> [v])
    }


testDimongVerices:: [String]
testDimongVerices = (dfsFold playTwoDimonds  (listChildVertices :: MonoidFoldAccLogic  String (String, String) [String]) "a0") -- :: tells compiler how to specialize polymorphic aggreagator
-- prints ["a0","a01","a3","a02"]

--
-- One more polymorphic aggregator
--
countDepth :: MonoidFoldAccLogic v e (Sum Int)
countDepth = defaultMonoidFoldAccLogic {
                 applyEdge   = const( Sum 1)
             }


testDimongGraphDepthCount:: Int
testDimongGraphDepthCount = getSum $ (dfsFold playTwoDimonds (countDepth :: MonoidFoldAccLogic v (v, v) (Sum Int)) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
