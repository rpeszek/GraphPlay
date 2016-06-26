--
--  This program contains non-memoized implementation of what we call in IPaC a (or the) GraphFold
--  This code is purely functional, no variable state is mutated.
--
--  To make it faster a directed graph traversal would need to remember all visited vertices
--  and return computation result from that vertex next time the vertex is visited
--  (e.g. memoization).  I have not figured out yet if that can
--  be done in a pure code (without any state mutation) maybe with something like a State Monad.
--  I have seen pure memoized implementation of things like Fibonacci
--  numbers but general d-graph topology is more complex that Fib (which is effectively a binary tree).
--
--  To move forward I will try to investigate pure and non-pure faster implementations
--  of this code.
--  I also will want to see if I can make this code even more polymorphic.
--  Notice that the implementation of dfsFold is a bunch of 'map'-s and function compositions (.).
--  These can be generalized (especially map).  So it will be interesting
--  to think if this code can be made even more general and thus stronger typed.
--
--  I added bunch of comments that should make reading this code easier for developers
--  who have not seen Haskell before.
--
--  Note: LANGUAGE extensions shown below are typically configured globally for a project
--
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}

module GraphPlay.DGraph where --exports everything, a terrible programmer wrote it

import Data.Hashable
import Control.Monad
import Control.Monad.ST
import GraphPlay.Memo
import Control.Lens
import Control.Monad (join)
import Data.List (nub)
import qualified Data.HashTable.Class as H

--
-- polymorphic type class defining directed graph
-- except g is not assumed to be a traversable or foldable
-- 
-- v (vertices) and e (edges) can be completely any types as long as we can define
-- resolveVertices and cEdgesOf functions.  cEdgesOf really imposes directed graph structure
-- This class definition is really a copy-paste of D-graph definition from a graph textbook.
--
-- a simple implementation or this polymorphic type (class of types) will be provided at the end.
-- NOTE:  a -> b -> c declares function of two arguments of type a and b returning type c
--
-- TODO This assumes that e can always be sematically resolved to (v,v)
-- TODO needs validVertex :: g -> v -> Bool
--
class DGraph g v e where
  resolveVertices ::  g -> e -> (v,v)  -- semantically resolves vertices edge does not need to be in the graph
  cEdgesOf   ::  g -> v -> [e]    -- return a list of child edges, empty if not a valid vertex

--
-- aggregator type that will be used for folding, notice arbitrary not specified types v (vertex) e (edge) and a (acc)
--
data DGAggregator v e a = DGAggregator {
    applyVertex :: v -> a -> a,        -- function that takes vertex and acc and returns new acc
    applyEdge :: e -> a -> a,          -- function that takes an edge and acc and returns new acc
    aggregate :: [a] -> a              -- function that combines several acc results into one (to be used across child results of a vertex)
}

--
-- helper type used internally, holds computation result for a vertex
--
data PartialFoldRes v e a = PartialFoldRes {_rvertex:: v, _redge:: e, _raccumulator:: a}
makeLenses ''PartialFoldRes

--
-- polymorphic DFS graphFold function, folds any implementation of polymorphic DGraph g starting at vertex v
-- using aggregator DGAggregator that aggregates to an arbitrary type a
--
-- Note RHS of => defines constraints (g v e form a DGraph)
-- '.' is function composition
-- :: defines a type to help compiler approve the code (polymporhic definition make Haskell often require explicitly specifying type in implementation)
-- \x -> expression    defines a lambda in Haskell
-- $ replaces '()' making code easier to read.  Instead of grouping x ( y z ) I can write x $ y z
-- 'over' mutates lens (like raccumulator defined in the helper type), 'view' is a lens getter
--
dfsFoldSlow :: forall g v e a. (DGraph g v e) => g -> v -> DGAggregator v e a  -> a
dfsFoldSlow g v logic =
    let _aggregate = aggregate logic       -- (:t) [a] -> a
        _applyVertex = applyVertex logic   -- (:t) v -> a -> a
        _applyEdge = applyEdge logic       -- (:t) e -> a -> a
        _childTempResults = map ((\ev -> PartialFoldRes{_rvertex = (_second ev), _redge = (_first ev), _raccumulator = (dfsFoldSlow g (_second ev) logic)})
                           . (\e -> (e, (_second . (resolveVertices g)) e))) (g `cEdgesOf` v) :: [PartialFoldRes v e a]
    in (_applyVertex v) . _aggregate $ map (view raccumulator)
          $ map (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults



dfsFoldST :: forall s g v e a. (Eq v, Hashable v, DGraph g v e) => ST s (HashTable s v a) -> g -> v -> DGAggregator v e a  -> ST s a
dfsFoldST h g v logic =
    let _aggregate = aggregate logic       :: [a] -> a
        _applyVertex = applyVertex logic   :: v -> a -> a
        _applyEdge = applyEdge logic       :: e -> a -> a
        _childEdges =  g `cEdgesOf` v      :: [e]
    in do
        _childTempResults <- forM _childEdges (\_childEdge -> do
              let _childVertex= (_second . (resolveVertices g)) _childEdge
              _childResult <- dfsFoldST h g _childVertex logic
              return PartialFoldRes{_rvertex = _childVertex, _redge = _childEdge, _raccumulator = _childResult}
         )
        return $ (_applyVertex v) . _aggregate $ map (view raccumulator)
            $ map (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults


runDtsFoldST :: forall s g v e a. (Eq v, Hashable v, DGraph g v e) => g -> v -> DGAggregator v e a  -> ST s a
runDtsFoldST g v logic = do
     ht <- H.new :: ST s (HashTable s v a)
     a <- dfsFoldST (return ht) g v logic
     return a

dfsFold :: forall g v e a. (Eq v, Hashable v, DGraph g v e) => g -> v -> DGAggregator v e a  -> a
dfsFold g v agg = runST $ runDtsFoldST g v agg
-- various helpers (typically exist defined somewhere else but wanted to limit imports)
-- remember (,) is a pair
--
_second :: (a,b) -> b
_second (_,x) = x

_first :: (a,b) -> a
_first (x,_) = x

--
-- converts function of one arg to function of 2 args which ignores the first argument
-- read declaration like so : (a -> b) -> (c -> a -> b)  (-> always associate to the right)
-- Brain teaser: as typical of me, I have named this function not very well.
--               in fact Haskell already has this function defined
--               .. it is called 'const'.  Why is that?
_onSecondArg :: (a -> b) -> c -> a -> b
_onSecondArg f _ a = f a
-- or this would also work (why?):
-- _onSecondArg = const

----------------------------------------------------------------------
-- tests

-- let's create a very simple (and slow)  of DGraph class for testing
-- Note: runSimpleGraph is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'runSimpleGraph sg'
--
newtype SimpleGraph v = SimpleGraph { runSimpleGraph:: [(v,v)]}

--
-- notice how this syntax defines how SimpleGraph is a DGraph
-- edges are defined as (v,v)
--
-- NOTICE: we need to find matching first element, so we need to have ==
-- not all types have that only types that implement Eq class do.
-- (this is not some Java, things are logical here)
--
instance forall v . (Eq v) => (DGraph (SimpleGraph v) v (v,v)) where
  resolveVertices g e = e                                                      --(:t) g -> e -> (v,v), brain teaser why is that?
  cEdgesOf g ver = filter (\vv -> _first vv == ver) . runSimpleGraph $ g  --(:t) g -> v -> [e]

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
testDimondGraph :: SimpleGraph String
testDimondGraph = SimpleGraph testEdges

-- testDimondGraph `cEdgesOf` "a0" :: [(String, String)]

--
-- example aggregator (polymorphic for arbitrary v and e types but to count a needs to be an Int or something of that sort)
--
countEdges :: DGAggregator v e Int
countEdges = DGAggregator {
       applyEdge   = _onSecondArg (+1),
       applyVertex = _onSecondArg id,
       aggregate   = sum
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = (dfsFold testDimondGraph "a0" (countEdges :: DGAggregator v (v, v) Int)) -- :: tells compiler how to specialize polymorphic aggregator
-- prints 4

-- another example aggregator (polymorphic)
-- NOTICE again to list non-duplicate vertices we need to be able to merge duplicates, hece Eq constraint
--
listChildVertices :: forall v e . (Eq v) => DGAggregator v e [v]
listChildVertices = DGAggregator {
       applyEdge   = _onSecondArg id,
       applyVertex = (:),  -- applying vertex is simply list prepend funtion that adds element to a list
       aggregate   = flattenUnique
    }

flattenUnique :: (Eq a) => [[a]] -> [a]
flattenUnique = nub . join  -- nub returns list of unique items (that is why Eq is needed)
                            -- join is basically flatten when applied to lists (it is more polymorphic of course)
                            -- '.' is function composition in Haskell (it is iteself a function of cause)

testDimongVerices:: [String]
testDimongVerices = (dfsFold testDimondGraph "a0" (listChildVertices :: DGAggregator String (String, String) [String])) -- :: tells compiler how to specialize polymorphic aggreagator
-- prints ["a0","a01","a3","a02"]

--
-- One more polymorphic aggregator
--
countDepth :: DGAggregator v e Int
countDepth = DGAggregator {
                 applyEdge   = _onSecondArg (+1),
                 applyVertex = _onSecondArg id,
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
testDimongGraphDepthCount = (dfsFold testDimondGraph "a0" (countDepth :: DGAggregator v (v, v) Int)) -- :: needs to define edge type
-- prints 2

tests = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples

--about 62 code lines
