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
import GraphPlay.DGraph.DfsMonoidFolds
import Control.Monad (join)
import Data.List (nub)


main :: IO ()
main = print experiments

-- simple test data (list of pars that will serve as edges)
testEdges = [
        ("a0", "a01"),
        ("a0", "a02"),
        ("a01", "a1"),
        ("a02", "a1"),
        ("a1", "a11"),
        ("a1", "a12"),
        ("a11", "a2"),
        ("a12", "a2")
     ]

--
-- notice SimpleGraph is not specialized to String type
--
testDimonGraph :: SimpleGraph String []
testDimonGraph = SimpleGraph testEdges

-- | Monoid under addition.
newtype Sum a = Sum { getSum :: a } deriving Show

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
--
countEdges ::  ChildFoldingAccLogic v e (Sum Int)
countEdges = ChildFoldingAccLogic {
       applyEdge   = const (Sum 1),
       applyVertex = const (Sum 0)
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = getSum $ (dfsFold testDimonGraph (countEdges :: ChildFoldingAccLogic v (v, v) (Sum Int)) "a0") -- :: tells compiler how to specialize polymorphic aggregator
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
testDimongVerices = (dfsFold testDimonGraph  (listChildVertices :: ChildFoldingAccLogic  String (String, String) [String]) "a0") -- :: tells compiler how to specialize polymorphic aggreagator
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
testDimongGraphDepthCount = getSum $ (dfsFold testDimonGraph (countDepth :: ChildFoldingAccLogic v (v, v) (Sum Int)) "a0") -- :: needs to define edge type
-- prints 2

experiments = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples
