--
--
--

module PolyGraph.DGraph.DfsFolds where --TODO exports everything, a terrible programmer wrote it

import Data.Hashable
import Control.Monad
import Control.Monad.ST
import Control.Lens
import qualified Data.HashTable.Class as H
import PolyGraph.Helpers
import PolyGraph.Memo
import PolyGraph.DGraph


--
-- aggregator type that will be used for folding
--
data ChildTraversingAccLogic t v e a = ChildTraversingAccLogic {
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
-- using aggregator ChildTraversingAccLogic that aggregates to an arbitrary type a
--
dfsFoldM :: forall m g v e t a. (Monad m, CIndex g v e t) => FoldOptimizer m v a -> g ->  ChildTraversingAccLogic t v e a  -> v -> m a
dfsFoldM optimizer g logic v =
    let _aggregate = aggregate logic       :: t a -> a
        _applyVertex = applyVertex logic   :: v -> a -> a
        _applyEdge = applyEdge logic       :: e -> a -> a
        _childEdges =  g `cEdgesOf` v      :: t e
    in do
        _childTempResults <- forM _childEdges (\_childEdge -> do
              let _childVertex= (second' . resolveVertices) _childEdge
              _childResult <- optimize optimizer (dfsFoldM optimizer g logic) $ _childVertex
              return PartialFoldRes{_rvertex = _childVertex, _redge = _childEdge, _raccumulator = _childResult}
         )
        return $ (_applyVertex v) . _aggregate $ fmap (view raccumulator)
            $ fmap (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults


--
-- This walks the grah without remembering visited vertices (effectively walks a tree)
-- will not work if DGraph has cycles
--
dfsFoldSlow :: forall g v e t a. (CIndex g v e t) => g -> ChildTraversingAccLogic t v e a  -> v -> a
dfsFoldSlow g logic v = let optimizer = FoldOptimizer { optimize = id } :: FoldOptimizer Identity v a
                        in runIdentity (dfsFoldM optimizer g logic v)

--
-- Uses memoization to assure that each vertex is visisted only once.  Will work with cycles.
--
dfsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => HashTable s v a -> g ->  ChildTraversingAccLogic t v e a  -> v -> ST s a
dfsFoldST h     = let optimizer = FoldOptimizer { optimize = memo h } :: FoldOptimizer (ST s) v a
                  in dfsFoldM optimizer

runDtsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  ChildTraversingAccLogic t v e a  -> v -> ST s a
runDtsFoldST g logic v = do
     ht <- H.new :: ST s (HashTable s v a)
     a <- dfsFoldST ht g logic v
     return a

dfsFoldFast :: forall g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  ChildTraversingAccLogic t v e a  -> v -> a
dfsFoldFast g agg v = runST $ runDtsFoldST g agg v

dfsFold :: forall g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  ChildTraversingAccLogic t v e a  -> v -> a
dfsFold = dfsFoldFast
