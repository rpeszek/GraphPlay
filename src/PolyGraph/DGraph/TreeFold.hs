--
-- Implements efficient fold of a tree expansion of the DGraph (DGraph d-paths)
-- where accumulation on each vertex v is computed only once (and memoized).
-- each vertex v is visited pe(v) times (number of parent edges of the folded subgraph)
--

module PolyGraph.DGraph.TreeFold where --TODO exports everything, a terrible programmer wrote it

import Data.Hashable (Hashable)
import Control.Monad (liftM, forM)
import Control.Monad.ST (ST, runST)
import Control.Lens
import qualified Data.HashTable.Class as HT
import qualified PolyGraph.Helpers as H
import PolyGraph.RecursionHelpers
import PolyGraph.DGraph


newtype AccError = AccError String
--
-- aggregator type that will be used in folding
--
data FoldAccLogic t v e a = FoldAccLogic {
      applyVertex :: v -> a -> a,        -- function that takes vertex and acc and returns new acc
      applyEdge   :: e -> a -> a,          -- function that takes an edge and acc and returns new acc
      aggregate   :: (Traversable t) => t a -> a,   -- function that combines several acc results into one (to be used across child results of a vertex)
      handleCycle :: v -> Either AccError a    -- what to do if cycle is detected
}


--
-- helper type used internally, holds computation result for a vertex
--
data PartialFoldRes v e a = PartialFoldRes {_rvertex:: v, _redge:: e, _raccumulator:: a}
makeLenses ''PartialFoldRes

--
-- polymorphic DFS graphFold function, folds any implementation of polymorphic CIndex g starting at vertex v
-- using aggregator FoldAccLogic that aggregates to an arbitrary type a
-- NOTE: aggregate Traversable needs to match CIndex Traversable
-- This is done for simplicity (aggregate almost directly consumes CIndex collection of edges)
--
dfsFoldM :: forall m g v e t a. (Monad m, CIndex g v e t) => RecursionHandler m v a -> g ->  FoldAccLogic t v e a  -> v -> m a
dfsFoldM handler g logic v =
    let _aggregate = aggregate logic       :: t a -> a
        _applyVertex = applyVertex logic   :: v -> a -> a
        _applyEdge = applyEdge logic       :: e -> a -> a
        _childEdges =  g `cEdgesOf` v      :: t e
    in do
        _childTempResults <- forM _childEdges (\_childEdge -> do
              let _childVertex= (H.second' . resolveVertices) _childEdge
              _childResult <- handle handler (dfsFoldM handler g logic) $ _childVertex
              return PartialFoldRes{_rvertex = _childVertex, _redge = _childEdge, _raccumulator = _childResult}
         )
        return $ (_applyVertex v) . _aggregate $ fmap (view raccumulator)
            $ fmap (\chres -> over (raccumulator) (_applyEdge( view redge chres)) chres ) _childTempResults


--
-- This walks the grah without remembering visited vertices (effectively walks a tree)
--
dfsFoldExponential :: forall g v e t a. (CIndex g v e t) => g -> FoldAccLogic t v e a  -> v -> a
dfsFoldExponential g logic v = let handler = RecursionHandler { handle = id } :: RecursionHandler Identity v a
                        in runIdentity (dfsFoldM handler g logic v)

--
-- Uses memoization to assure that each vertex is visisted only once.  Will currently not work with cycles.
--
dfsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => HashTable s v Bool -> HashTable s v a -> g ->  FoldAccLogic t v e a  -> v -> ST s a
dfsFoldST htCycles htmemo g logic =
              let  cyclesHandler :: v -> ST s a
                   cyclesHandler v =  do
                              let aOrError = handleCycle logic v
                              case aOrError of Right a -> return a
                                               Left (AccError msg) -> fail msg

                   handler = RecursionHandler { handle = (memo htmemo) . (handleReentry htCycles cyclesHandler) } :: RecursionHandler (ST s) v a
              in dfsFoldM handler g logic

runDtsFoldST :: forall s g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  FoldAccLogic t v e a  -> v -> ST s a
runDtsFoldST g logic v = do
     htCycles <- HT.new :: ST s (HashTable s v Bool)
     htmemo <- HT.new :: ST s (HashTable s v a)
     a <- dfsFoldST htCycles htmemo g logic v
     return a

dfsFoldFast :: forall g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  FoldAccLogic t v e a  -> v -> a
dfsFoldFast g agg v = runST $ runDtsFoldST g agg v

dfsFold :: forall g v e t a. (Eq v, Hashable v, CIndex g v e t) => g ->  FoldAccLogic t v e a  -> v -> a
dfsFold = dfsFoldFast
