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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module GraphPlay.DGraph where --exports everything, a terrible programmer wrote it

import Data.Hashable
import Control.Monad
import Control.Monad.ST
import GraphPlay.Memo
import Control.Lens
import Control.Monad (join)
import Data.List (nub)
import qualified Data.HashTable.Class as H


class DEdgeSemantics e v where
  resolveVertices ::  e -> (v,v)  -- semantically resolves vertices edge does not need to be in the graph

--
-- one who directs directed graph :) - in child direction
--
class (DEdgeSemantics e v)  => DirectorC g v e where
  cEdgesOf   ::  g -> v -> [e]   -- return a list of child edges, empty if not a valid vertex or a leaf

class (DEdgeSemantics e v, Eq v)  =>  DGraph g v e where
  validVetex ::  g -> v -> Bool
  edgesOf    ::  g -> v -> [e]   -- return a of all edges, empty if not a valid vertex or disconnected vertex

-- let's create a very simple (and slow)  of DirectorC class for testing
-- Note: runSimpleGraph is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'runSimpleGraph sg'
--
newtype SimpleGraph v = SimpleGraph { runSimpleGraph:: [(v,v)]}

--
-- instances
--
instance forall g v e . (DGraph g v e) => DirectorC g v e where
   cEdgesOf g v = filter (\e -> v == ((first' . resolveVertices) e)) (edgesOf g v)

instance forall v . (Eq v) => (DEdgeSemantics  (v,v) v) where
  resolveVertices e = e                                                   --(:t) g -> e -> (v,v), brain teaser why is that?

instance forall v . (Eq v) => (DirectorC (SimpleGraph v) v (v,v)) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . runSimpleGraph $ g  --(:t) g -> v -> [e]


-- HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit imports)
--
second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x


------------ TODO move out   --
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
-- polymorphic DFS graphFold function, folds any implementation of polymorphic DirectorC g starting at vertex v
-- using aggregator DGAggregator that aggregates to an arbitrary type a
--
-- Note RHS of => defines constraints (g v e form a DirectorC)
-- '.' is function composition
-- :: defines a type to help compiler approve the code (polymporhic definition make Haskell often require explicitly specifying type in implementation)
-- \x -> expression    defines a lambda in Haskell
-- $ replaces '()' making code easier to read.  Instead of grouping x ( y z ) I can write x $ y z
-- 'over' mutates lens (like raccumulator defined in the helper type), 'view' is a lens getter
--
dfsFoldSlow :: forall g v e a. (DirectorC g v e) => g -> v -> DGAggregator v e a  -> a
dfsFoldSlow g v logic =
    let _aggregate = aggregate logic       -- (:t) [a] -> a
        _applyVertex = applyVertex logic   -- (:t) v -> a -> a
        _applyEdge = applyEdge logic       -- (:t) e -> a -> a
        _childTempResults = map ((\ev -> PartialFoldRes{_rvertex = (second' ev), _redge = (first' ev), _raccumulator = (dfsFoldSlow g (second' ev) logic)})
                           . (\e -> (e, (second' . resolveVertices) e))) (g `cEdgesOf` v) :: [PartialFoldRes v e a]
    in (_applyVertex v) . _aggregate $ map (view raccumulator)
          $ map (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults



dfsFoldST :: forall s g v e a. (Eq v, Hashable v, DirectorC g v e) => ST s (HashTable s v a) -> g -> v -> DGAggregator v e a  -> ST s a
dfsFoldST h g v logic =
    let _aggregate = aggregate logic       :: [a] -> a
        _applyVertex = applyVertex logic   :: v -> a -> a
        _applyEdge = applyEdge logic       :: e -> a -> a
        _childEdges =  g `cEdgesOf` v      :: [e]
    in do
        _childTempResults <- forM _childEdges (\_childEdge -> do
              let _childVertex= (second' . resolveVertices) _childEdge
              _childResult <- dfsFoldST h g _childVertex logic
              return PartialFoldRes{_rvertex = _childVertex, _redge = _childEdge, _raccumulator = _childResult}
         )
        return $ (_applyVertex v) . _aggregate $ map (view raccumulator)
            $ map (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults


runDtsFoldST :: forall s g v e a. (Eq v, Hashable v, DirectorC g v e) => g -> v -> DGAggregator v e a  -> ST s a
runDtsFoldST g v logic = do
     ht <- H.new :: ST s (HashTable s v a)
     a <- dfsFoldST (return ht) g v logic
     return a

dfsFold :: forall g v e a. (Eq v, Hashable v, DirectorC g v e) => g -> v -> DGAggregator v e a  -> a
dfsFold g v agg = runST $ runDtsFoldST g v agg

-- HELPERS
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
countEdges :: DGAggregator v e Int
countEdges = DGAggregator {
       applyEdge   = _onSecondArg (+1),
       applyVertex = _onSecondArg id,
       aggregate   = sum
    }

testDimongGraphEdgeCount:: Int
testDimongGraphEdgeCount = (dfsFold testDimonGraph "a0" (countEdges :: DGAggregator v (v, v) Int)) -- :: tells compiler how to specialize polymorphic aggregator
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
testDimongVerices = (dfsFold testDimonGraph "a0" (listChildVertices :: DGAggregator String (String, String) [String])) -- :: tells compiler how to specialize polymorphic aggreagator
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
testDimongGraphDepthCount = (dfsFold testDimonGraph "a0" (countDepth :: DGAggregator v (v, v) Int)) -- :: needs to define edge type
-- prints 2

tests = [show testDimongGraphDepthCount, show testDimongGraphEdgeCount, show testDimongVerices]
-- prints all examples

--about 62 code lines
