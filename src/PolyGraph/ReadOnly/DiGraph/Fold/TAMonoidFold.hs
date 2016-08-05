--
-- Smilar to TAFold but use different accumulator types based on Monoid
-- Efficient momoized fold of DiGraph which behaves as a Tree fold because of how vertices aggregate edge results.
-- Each graph vertex is computed only once.
--

module PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold (
  AccError (..)
  , MonoidFoldAccLogic (..)
  , defaultMonoidFoldAccLogic
  , dfsFold
  , dfsFoldFast
  , dfsFoldExponential
) where 

import Control.Lens
import Data.Hashable
import Control.Monad                    (liftM, foldM)
import Control.Monad.ST                 (ST, runST)
import qualified Data.HashTable.Class   as HT
import qualified PolyGraph.Common       as Common
import PolyGraph.ReadOnly.DiGraph       (DiAdjacencyIndex(..), DiEdgeSemantics(..))
import qualified PolyGraph.Common.RecursionHelpers as RecHelp


newtype AccError = AccError String
--
-- aggregator type that will be used for folding
--
data MonoidFoldAccLogic v e a = MonoidFoldAccLogic {
      applyVertex :: v -> a,        -- function that takes vertex and returns monoid
      applyEdge   :: e -> a,        -- function that takes an edge and returns monoid
      handleCycle :: v -> Either AccError a    -- what to do if cycle is detected
}

defaultMonoidFoldAccLogic :: forall v e a. (Monoid a) => MonoidFoldAccLogic v e a
defaultMonoidFoldAccLogic = MonoidFoldAccLogic {
      applyVertex = const mempty,
      applyEdge   = const mempty,
      handleCycle = const $ Left (AccError "Cycle detected")
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
dfsFoldM :: forall m g v e t a. (Monoid a, Monad m, DiAdjacencyIndex g v e t) => RecHelp.RecursionHandler m v a -> g ->  MonoidFoldAccLogic v e a  -> v -> m a
dfsFoldM handler g logic v =
     let acc_applyVertex =  applyVertex logic v   :: a
         acc_applyEdge   =  applyEdge   logic     :: e -> a
         _recursionV     =  RecHelp.handle handler (dfsFoldM handler g logic)   :: v -> m a
         _recursionE     = _recursionV . Common.second . resolveDiEdge       :: e -> m a
         _recursion      = (\e -> (liftPairHelper e) . _recursionE $ e) :: e -> m (e, a)
         _childEdgesM    =  g `cEdgesOf` v                              :: t e
         _foldedChildResults =
                        (mapM _recursion _childEdgesM) >>=
                        (foldM (\a ea -> return $ (acc_applyEdge (Common.pairFirst ea)) `mappend` (Common.pairSecond ea) `mappend` a) mempty)
         _finalResult = (liftM (mappend acc_applyVertex)) _foldedChildResults  :: m a
     in
         _finalResult

--
-- currently not used
-- 
_dfsFoldMS :: forall m g v e t a. (Monoid a, Monad m, DiAdjacencyIndex g v e t, Show v) => RecHelp.RecursionHandler (Common.GraphApp m) v a -> g ->  MonoidFoldAccLogic v e a  -> v -> Common.GraphApp m a
_dfsFoldMS handler g logic v =
     let acc_applyVertex =  applyVertex logic v   :: a
         acc_applyEdge   =  applyEdge   logic     :: e -> a
         _recursionV     =  RecHelp.handle handler (_dfsFoldMS handler g logic)   :: v ->  Common.GraphApp m a
         _recursionE     = _recursionV . Common.second . resolveDiEdge           :: e ->  Common.GraphApp m a
         _recursion      = (\e -> (liftPairHelper e) . _recursionE $ e) :: e ->  Common.GraphApp m (e, a)
         _childEdgesM    =  g `cEdgesOf` v                              :: t e
         _               = do Common.trace "visited" (show v) :: Common.GraphApp m ()
         _foldedChildResults =
                        (mapM _recursion _childEdgesM) >>=
                        (foldM (\a ea -> return $ (acc_applyEdge (Common.pairFirst ea)) `mappend` (Common.pairSecond ea) `mappend` a) mempty)
         _finalResult = (liftM (mappend acc_applyVertex)) _foldedChildResults  :: Common.GraphApp m a
     in
         _finalResult

--
-- This walks the grah without remembering visited vertices (effectively walks a tree)
-- will not work if DiGraph has cycles, and will be slow
--
dfsFoldExponential :: forall g v e t a. (Monoid a, DiAdjacencyIndex g v e t) => g -> MonoidFoldAccLogic v e a  -> v -> a
dfsFoldExponential g logic v =
                        let handler = RecHelp.RecursionHandler { RecHelp.handle = id } :: RecHelp.RecursionHandler Identity v a
                        in runIdentity (dfsFoldM handler g logic v)

-- TODO the following boilerplate is almost the same as in TreeFold externalize it for code reuse
--
-- Uses memoization to assure that each vertex is visisted only once.  Will currently not work with cycles.
--
dfsFoldST :: forall s g v e t a. (Monoid a, Eq v, Hashable v, DiAdjacencyIndex g v e t) => RecHelp.HashTable s v Bool -> RecHelp.HashTable s v a -> g ->  MonoidFoldAccLogic v e a  -> v -> ST s a
dfsFoldST htCycles htmemo g logic =
              let  cyclesHandler :: v -> ST s a
                   cyclesHandler v =  do
                              let aOrError = handleCycle logic v
                              case aOrError of Right a -> return a
                                               Left (AccError msg) -> fail msg

                   handler = RecHelp.RecursionHandler { RecHelp.handle = (RecHelp.memo htmemo) . (RecHelp.handleReentry htCycles cyclesHandler) } :: RecHelp.RecursionHandler (ST s) v a
              in dfsFoldM handler g logic


runDtsFoldST :: forall s g v e t a. (Monoid a, Eq v, Hashable v, DiAdjacencyIndex g v e t) => g ->  MonoidFoldAccLogic  v e a  -> v -> ST s a
runDtsFoldST g logic v = do
     htCycles <- HT.new :: ST s (RecHelp.HashTable s v Bool)
     htmemo <- HT.new :: ST s   (RecHelp.HashTable s v a)
     a <- dfsFoldST htCycles htmemo g logic v
     return a

dfsFoldFast :: forall g v e t a. (Monoid a, Eq v, Hashable v, DiAdjacencyIndex g v e t) => g ->  MonoidFoldAccLogic v e a  -> v -> a
dfsFoldFast g agg v = runST $ runDtsFoldST g agg v

dfsFold :: forall g v e t a. (Monoid a, Eq v, Hashable v, DiAdjacencyIndex g v e t) => g ->  MonoidFoldAccLogic v e a  -> v -> a
dfsFold = dfsFoldFast
