module Play.DiGraph.TreeMonoidFoldOnSimpleGraph where

----------------------------------------------------------------------
-- test experiments

import Data.Hashable
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Common
import PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold
import qualified Data.HashSet as HS
import Play.DiGraph.SampleData (playTwoDiamonds)


-- | Numbers as Monoids under addition.
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
testDimongGraphEdgeCount = getSum $ (dfsFold playTwoDiamonds (countTreeEdges :: MonoidFoldAccLogic v (OPair v) (Sum Int)) "a0") -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

--
-- NOTE if I used [] instead of HashSet this aggregator would not scale (would have exp cost)
--
listChildVertices :: forall v e . (Hashable v, Eq v) => MonoidFoldAccLogic v e (HS.HashSet v)
listChildVertices = defaultMonoidFoldAccLogic {
                       applyVertex = (\ v -> HS.singleton v)
                    }


testDimongVerices:: [String]
testDimongVerices = HS.toList (dfsFold playTwoDiamonds (listChildVertices :: MonoidFoldAccLogic  String (OPair String) (HS.HashSet String)) "a0")
-- :: Note need to tell compiler how to specialize polymorphic aggreagator
-- prints all vertices

--
-- One more polymorphic aggregator
--
countDepth :: MonoidFoldAccLogic v e (Sum Int)
countDepth = defaultMonoidFoldAccLogic {
                 applyEdge   = const( Sum 1)
             }


testDimongGraphDepthCount:: Int
testDimongGraphDepthCount = getSum $ (dfsFold playTwoDiamonds (countDepth :: MonoidFoldAccLogic v (OPair v) (Sum Int)) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
