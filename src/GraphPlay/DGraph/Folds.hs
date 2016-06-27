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

module GraphPlay.DGraph.Folds where --TODO exports everything, a terrible programmer wrote it

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
