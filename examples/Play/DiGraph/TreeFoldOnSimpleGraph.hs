module Play.DiGraph.TreeFoldOnSimpleGraph where

import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Common.Helpers
import PolyGraph.ReadOnly.DiGraph.Fold.TAFold
import Data.Monoid
import Data.Hashable
import qualified Data.HashSet as HS
import PolyGraph.Instances.SimpleGraph
import Play.DiGraph.SampleData (playTwoDiamondsSetGraph, playTwoDiamonds)

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
-- this counts edges of a tree representation of d-graph
-- Compuation is efficient (done on the graph, not on the tree)
--
countTreeEdges :: FoldAccLogic [] v e Int
countTreeEdges = FoldAccLogic {
       applyEdge   = const (+1),
       applyVertex = const id,
       aggregate   = sum,
       handleCycle = const $ Left (AccError "Cycle detected. That would be infite tree")
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = (dfsFold playTwoDiamonds (countTreeEdges :: FoldAccLogic [] v (OPair v) Int) "a0") -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

--
-- NOTE if I used [] instead of HashSet this aggregator would not scale (would have exp cost)
--
listChildVertices :: forall v e . (Hashable v, Eq v) => FoldAccLogic [] v e (HS.HashSet v)
listChildVertices = FoldAccLogic {
       applyEdge   = const id,
       applyVertex = HS.insert,  -- applying vertex is simply list prepend funtion that adds element to a list
       aggregate   = mconcat,
       handleCycle = const $ Right mempty
    }

testDimongVerices:: [String]
testDimongVerices = HS.toList (dfsFold playTwoDiamonds  (listChildVertices :: FoldAccLogic [] String (OPair String) (HS.HashSet String)) "a0") -- :: tells compiler how to specialize polymorphic aggreagator
-- prints ["a0","a01","a3","a02"]

--
-- One more polymorphic aggregator
--
countDepth :: FoldAccLogic [] v e Int
countDepth = FoldAccLogic {
                 applyEdge   = const (+1),
                 applyVertex = const id,
                 aggregate   = safeListMax 0,
                 handleCycle = const $ Left (AccError "Cycle Detected")
             }

--
-- finds largest element (that is why we have Ord constraint)
-- for first argument if that one is larger
-- NOTICE Haskell elegant pattern matching
--
safeListMax :: (Ord a) => a -> [a] -> a
safeListMax a []     = a                        -- for empty list
safeListMax a (x:xs) = max x (safeListMax a xs) -- for non-emtpy list starting with x

testDimongGraphDepthCount:: Int
testDimongGraphDepthCount = (dfsFold playTwoDiamonds (countDepth :: FoldAccLogic [] v (OPair v) Int) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
