module Play.DGraph.TreeFoldOnSimpleGraph where

----------------------------------------------------------------------
-- test experiments

import PolyGraph.DGraph
import PolyGraph.Helpers
import PolyGraph.DGraph.TreeFold
import Control.Monad (join)
import Data.List (nub)
import Play.DGraph.Types
import Play.DGraph.Samples (playTwoDimonds)

-- playTwoDimonds `cEdgesOf` "a0" :: [(String, String)]

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
-- this counts edges as if graph was expanded to a tree
--
countTreeEdges :: FoldAccLogic [] v e Int
countTreeEdges = FoldAccLogic {
       applyEdge   = const (+1),
       applyVertex = const id,
       aggregate   = sum,
       handleCycle = const $ Left (AccError "Cycle detected. That would be infite tree")
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = (dfsFold playTwoDimonds (countTreeEdges :: FoldAccLogic [] v (v, v) Int) "a0") -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

-- another example aggregator (polymorphic)
-- NOTICE again to list non-duplicate vertices we need to be able to merge duplicates, hece Eq constraint
--
listChildVertices :: forall v e . (Eq v) => FoldAccLogic [] v e [v]
listChildVertices = FoldAccLogic {
       applyEdge   = const id,
       applyVertex = (:),  -- applying vertex is simply list prepend funtion that adds element to a list
       aggregate   = flattenUnique,
       handleCycle = const $ Right []
    }

flattenUnique :: (Eq a) => [[a]] -> [a]
flattenUnique = nub . join  -- nub returns list of unique items (that is why Eq is needed)
                            -- join is basically flatten when applied to lists (it is more polymorphic of course)
                            -- '.' is function composition in Haskell (it is iteself a function of cause)

testDimongVerices:: [String]
testDimongVerices = (dfsFold playTwoDimonds  (listChildVertices :: FoldAccLogic [] String (String, String) [String]) "a0") -- :: tells compiler how to specialize polymorphic aggreagator
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
testDimongGraphDepthCount = (dfsFold playTwoDimonds (countDepth :: FoldAccLogic [] v (v, v) Int) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
