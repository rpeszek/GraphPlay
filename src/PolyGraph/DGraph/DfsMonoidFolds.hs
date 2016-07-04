--
--
--

module PolyGraph.DGraph.DfsMonoidFolds where --TODO exports everything, a terrible programmer wrote it

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
data ChildFoldingAccLogic v e a = ChildFoldingAccLogic {
      applyVertex :: v -> a,        -- function that takes vertex and returns monoid
      applyEdge   :: e -> a         -- function that takes an edge and returns monoid
}


--
-- helper type used internally, holds computation result for a vertex
--
data PartialFoldRes v e a = PartialFoldRes {_rvertex:: v, _redge:: e, _raccumulator:: a}
makeLenses ''PartialFoldRes

liftPairHelper :: forall m e x . (Monad m)=> e -> m x -> m (e,x)
liftPairHelper e1 =  liftM $ (,) e1

--
--
dfsFoldM :: forall m g v e t a. (Monoid a, Monad m, CIndex g v e t) => FoldOptimizer m v a -> g ->  ChildFoldingAccLogic v e a  -> v -> m a
dfsFoldM optimizer g logic v =
     let acc_applyVertex =  applyVertex logic v   :: a
         acc_applyEdge   =  applyEdge   logic     :: e -> a
         _recursionV     =  optimize optimizer (dfsFoldM optimizer g logic)   :: v -> m a
         _recursionE     = _recursionV . second' . resolveVertices            :: e -> m a
         _recursion      = (\e -> (liftPairHelper e) . _recursionE $ e)       :: e -> m (e, a)
         _childEdgesM    =  g `cEdgesOf` v                                    :: t e
         _foldedChildResults =
                        (mapM _recursion _childEdgesM) >>=
                        (foldM (\a ea -> return $ (acc_applyEdge (first' ea)) `mappend` (second' ea) `mappend` a) mempty)
         _finalResult = (liftM (mappend acc_applyVertex)) _foldedChildResults  :: m a
     in
         _finalResult

--
-- This walks the grah without remembering visited vertices (effectively walks a tree)
-- will not work if DGraph has cycles
--
dfsFoldSlow :: forall g v e t a. (Monoid a, CIndex g v e t) => g -> ChildFoldingAccLogic v e a  -> v -> a
dfsFoldSlow g logic v = let optimizer = FoldOptimizer { optimize = id } :: FoldOptimizer Identity v a
                        in runIdentity (dfsFoldM optimizer g logic v)

--
-- Uses memoization to assure that each vertex is visisted only once.  Will work with cycles.
--
dfsFoldST :: forall s g v e t a. (Monoid a, Eq v, Hashable v, CIndex g v e t) => HashTable s v a -> g ->  ChildFoldingAccLogic v e a  -> v -> ST s a
dfsFoldST h     = let optimizer = FoldOptimizer { optimize = memo h } :: FoldOptimizer (ST s) v a
                  in dfsFoldM optimizer

runDtsFoldST :: forall s g v e t a. (Monoid a, Eq v, Hashable v, CIndex g v e t) => g ->  ChildFoldingAccLogic v e a  -> v -> ST s a
runDtsFoldST g logic v = do
     ht <- H.new :: ST s (HashTable s v a)
     a <- dfsFoldST ht g logic v
     return a

dfsFoldFast :: forall g v e t a. (Monoid a, Eq v, Hashable v, CIndex g v e t) => g ->  ChildFoldingAccLogic v e a  -> v -> a
dfsFoldFast g agg v = runST $ runDtsFoldST g agg v

dfsFold :: forall g v e t a. (Monoid a, Eq v, Hashable v, CIndex g v e t) => g ->  ChildFoldingAccLogic v e a  -> v -> a
dfsFold = dfsFoldFast