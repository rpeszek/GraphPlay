--
--
--
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module GraphPlay.DGraph.DfsFolds where --TODO exports everything, a terrible programmer wrote it

import Data.Hashable
import Control.Monad
import Control.Monad.ST
import Control.Lens
import qualified Data.HashTable.Class as H
import GraphPlay.Helpers
import GraphPlay.Memo
import GraphPlay.DGraph


--
-- aggregator type that will be used for folding, notice arbitrary not specified types v (vertex) e (edge) and a (acc)
--
{-}
data DGAggregator t v e a where
  DGAggregator :: {
     applyVertex :: v -> a -> a,        -- function that takes vertex and acc and returns new acc
     applyEdge :: e -> a -> a,          -- function that takes an edge and acc and returns new acc
     aggregate :: (Traversable t) => t a -> a              -- function that combines several acc results into one (to be used across child results of a vertex)
  } -> DGAggregator t v e a
  -}
data DGAggregator t v e a = DGAggregator {
      applyVertex :: v -> a -> a,        -- function that takes vertex and acc and returns new acc
      applyEdge :: e -> a -> a,          -- function that takes an edge and acc and returns new acc
      aggregate :: (Traversable t) => t a -> a               -- function that combines several acc results into one (to be used across child results of a vertex)
}

--
-- helper type used internally, holds computation result for a vertex
--
data PartialFoldRes v e a = PartialFoldRes {_rvertex:: v, _redge:: e, _raccumulator:: a}
makeLenses ''PartialFoldRes

--
-- polymorphic DFS graphFold function, folds any implementation of polymorphic CIndex g starting at vertex v
-- using aggregator DGAggregator that aggregates to an arbitrary type a
--
-- Note RHS of => defines constraints (g v e form a CIndex)
-- '.' is function composition
-- :: defines a type to help compiler approve the code (polymporhic definition make Haskell often require explicitly specifying type in implementation)
-- \x -> expression    defines a lambda in Haskell
-- $ replaces '()' making code easier to read.  Instead of grouping x ( y z ) I can write x $ y z
-- 'over' mutates lens (like raccumulator defined in the helper type), 'view' is a lens getter
--
dfsFoldSlow :: forall g v e t a. (CIndex g v e t) => g -> DGAggregator t v e a  -> v -> a
dfsFoldSlow g logic v =
    let _aggregate = aggregate logic       -- (:t) [a] -> a
        _applyVertex = applyVertex logic   -- (:t) v -> a -> a
        _applyEdge = applyEdge logic       -- (:t) e -> a -> a
        _childTempResults = fmap ((\ev -> PartialFoldRes{_rvertex = (second' ev), _redge = (first' ev), _raccumulator = (dfsFoldSlow g logic (second' ev))})
                           . (\e -> (e, (second' . resolveVertices) e))) (g `cEdgesOf` v) :: t (PartialFoldRes v e a)
    in (_applyVertex v) . _aggregate $ fmap (view raccumulator)
          $ fmap (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults

--
--TODO understand why formM did not need to change when [] became t
--
dfsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => HashTable s v a -> g ->  DGAggregator t v e a  -> v -> ST s a
dfsFoldST h g logic v =
    let _aggregate = aggregate logic       :: t a -> a
        _applyVertex = applyVertex logic   :: v -> a -> a
        _applyEdge = applyEdge logic       :: e -> a -> a
        _childEdges =  g `cEdgesOf` v      :: t e
    in do
        _childTempResults <- forM _childEdges (\_childEdge -> do
              let _childVertex= (second' . resolveVertices) _childEdge
              _childResult <- memo h (dfsFoldST h g logic) $ _childVertex
              return PartialFoldRes{_rvertex = _childVertex, _redge = _childEdge, _raccumulator = _childResult}
         )
        return $ (_applyVertex v) . _aggregate $ fmap (view raccumulator)
            $ fmap (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults


runDtsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  DGAggregator t v e a  -> v -> ST s a
runDtsFoldST g logic v = do
     ht <- H.new :: ST s (HashTable s v a)
     a <- dfsFoldST ht g logic v
     return a

dfsFold :: forall g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  DGAggregator t v e a  -> v -> a
dfsFold g agg v = runST $ runDtsFoldST g agg v
