{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}


----------------------------------------------------------------------
-- test experiments

import GraphPlay.DGraph
import GraphPlay.Helpers
import GraphPlay.DGraph.Folds
import Control.Monad (join)
import Data.List (nub)

-- simple test data (list of pars that will serve as edges)
testEdges = [
        ("a0", "a01"),
        ("a0", "a02"),
        ("a01", "a3"),
        ("a02", "a3")
     ]

--
-- notice SimpleGraph is not specialized to String type
--
testDimonGraph :: SimpleGraph String
testDimonGraph = SimpleGraph testEdges

-- testDimonGraph `cEdgesOf` "a0" :: [(String, String)]

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
--
countEdges :: DGAggregator [] v e Int
countEdges = DGAggregator {
       applyEdge   = const (+1),
       applyVertex = const id,
       aggregate   = sum
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = (dfsFold testDimonGraph "a0" (countEdges :: DGAggregator [] v (v, v) Int)) -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

-- another example aggregator (polymorphic)
-- NOTICE again to list non-duplicate vertices we need to be able to merge duplicates, hece Eq constraint
--
listChildVertices :: forall v e . (Eq v) => DGAggregator [] v e [v]
listChildVertices = DGAggregator {
       applyEdge   = const id,
       applyVertex = (:),  -- applying vertex is simply list prepend funtion that adds element to a list
       aggregate   = flattenUnique
    }

flattenUnique :: (Eq a) => [[a]] -> [a]
flattenUnique = nub . join  -- nub returns list of unique items (that is why Eq is needed)
                            -- join is basically flatten when applied to lists (it is more polymorphic of course)
                            -- '.' is function composition in Haskell (it is iteself a function of cause)

testDimongVerices:: [String]
testDimongVerices = (dfsFold testDimonGraph "a0" (listChildVertices :: DGAggregator [] String (String, String) [String])) -- :: tells compiler how to specialize polymorphic aggreagator
-- prints ["a0","a01","a3","a02"]

--
-- One more polymorphic aggregator
--
countDepth :: DGAggregator [] v e Int
countDepth = DGAggregator {
                 applyEdge   = const (+1),
                 applyVertex = const id,
                 aggregate   =  safeListMax 0
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
testDimongGraphDepthCount = (dfsFold testDimonGraph "a0" (countDepth :: DGAggregator [] v (v, v) Int)) -- :: needs to define edge type
-- prints 2

tests = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples

--about 62 code lines
