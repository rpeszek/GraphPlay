module Play.DGraph.MonoidFoldsOnSimpleGraph where

----------------------------------------------------------------------
-- test experiments

import PolyGraph.DGraph
import PolyGraph.Helpers
import PolyGraph.DGraph.DAGMonoidFolds
import Control.Monad (join)
import Data.List (nub)
import Play.DGraph.Types
import Play.DGraph.Samples (playTwoDimonds)


-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a } deriving Show

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
--
countEdgesAsOnTree ::  ChildFoldingAccLogic v e (Sum Int)
countEdgesAsOnTree = ChildFoldingAccLogic {
       applyEdge   = const (Sum 1),
       applyVertex = const (Sum 0)
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = getSum $ (dfsFold playTwoDimonds (countEdgesAsOnTree :: ChildFoldingAccLogic v (v, v) (Sum Int)) "a0") -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

--
-- THIS LIST will have duplicates
--
listChildVertices :: forall v e . (Eq v) => ChildFoldingAccLogic v e [v]
listChildVertices = ChildFoldingAccLogic {
       applyEdge   = const (mempty :: [v]),
       applyVertex = (\v -> [v])
    }


testDimongVerices:: [String]
testDimongVerices = (dfsFold playTwoDimonds  (listChildVertices :: ChildFoldingAccLogic  String (String, String) [String]) "a0") -- :: tells compiler how to specialize polymorphic aggreagator
-- prints ["a0","a01","a3","a02"]

--
-- One more polymorphic aggregator
--
countDepth :: ChildFoldingAccLogic v e (Sum Int)
countDepth = ChildFoldingAccLogic {
                 applyEdge   = const( Sum 1),
                 applyVertex = const( Sum 0)
             }


testDimongGraphDepthCount:: Int
testDimongGraphDepthCount = getSum $ (dfsFold playTwoDimonds (countDepth :: ChildFoldingAccLogic v (v, v) (Sum Int)) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
