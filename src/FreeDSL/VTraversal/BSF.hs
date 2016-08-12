
module FreeDSL.VTraversal.BSF (
  runBSFState
  , runBSF
) where
  
import Data.Hashable
--import Control.Monad
--import Data.Maybe (maybe)
import Control.Monad.Free (Free(..))
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)
import qualified FreeDSL.VTraversal as DSL
import qualified PolyGraph.Common.NonBlockingQueue as Q
import Control.Monad.State (State, execState, evalState, modify, get, put)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- TODO need remainingNeighbors! all adjacent vertices need to be visited before being enqueued
-- this will start with (NoMore, Nothing, [], queue with root, emptySet, emptyMap)
-- State (currentObservation, Maybe workingFromV, workingNeighbors, queue, visited, storage)


runBSFState :: forall a g v e t r . (Hashable v, Eq v, AdjacencyIndex g v e t) => 
                               DSL.VTraversal a v r -> g -> HM.HashMap v a
runBSFState program g  = 
                        let initdata = (DSL.NoMore, Nothing, [], Q.emptyQueue, HS.empty, HM.empty)
                            (_,_,_,_,_,hm) = execState (interpretBSF g program) initdata
                        in hm

runBSF :: forall a g v e t . (Hashable v, Eq v, AdjacencyIndex g v e t) => 
                               DSL.VTraversal a v a -> g -> a
runBSF program g  = evalState (interpretBSF g program) (DSL.NoMore, Nothing, [], Q.emptyQueue, HS.empty, HM.empty)

type BSFState v a = (DSL.VObservation v, Maybe v, [v], Q.SimpleQueue v, HS.HashSet v, HM.HashMap v a)

interpretBSF :: forall a g v e t r . (Hashable v, Eq v, AdjacencyIndex g v e t) => 
                               g -> DSL.VTraversal a v r -> State (BSFState v a) r

interpretBSF g (Free (DSL.StartAt root next)) = (put (DSL.NoMore, Nothing, [], Q.enqueue root Q.emptyQueue, HS.empty, HM.empty)) >> interpretBSF g next
interpretBSF g (Free (DSL.NextVObs vNext)) = do
             s <- get
             let (obs,ns) = traversalHelper g s
             put ns
             interpretBSF g (vNext obs)
interpretBSF g (Free (DSL.CurrentVObs vNext)) = do
             (o,_, _, _, _, _) <- get
             interpretBSF g (vNext o)
interpretBSF g (Free (DSL.Put va next)) = (modify (putHelper va)) >> interpretBSF g next
interpretBSF g (Free (DSL.Get v aNext)) = do
             (_, _, _, _, _, hm) <- get
             let a = HM.lookup v hm
             interpretBSF g (aNext a)
interpretBSF _ (Pure r) = return r

putHelper :: (Hashable v, Eq v) => (v, a) -> BSFState v a -> BSFState v a
putHelper (v0, a) (o, v, vs, queue, vlist, hm) = (o, v, vs, queue, vlist, HM.insert v0 a hm)

traversalHelper :: forall a g v e t . (AdjacencyIndex g v e t, Hashable v, Eq v) =>
                 g -> BSFState v a -> (DSL.VObservation v, BSFState v a)
-- v is marked visited when is processed all elements in the queue are visited
traversalHelper g (_, Nothing, _, queue, visited, hm) = 
      let (newV, newQ) = Q.dequeue queue
          neighborVs = HS.fromList . (neighborsOf g)
          notVisitedVs v0 = HS.toList $ HS.difference (neighborVs v0) visited
      in case newV of 
         Nothing -> (DSL.NoMore, (DSL.NoMore, Nothing, [], newQ, visited, hm))
         Just v0 -> traversalHelper g (DSL.NoMore, newV, notVisitedVs v0, newQ, visited, hm)

traversalHelper g (_, Just _, [], queue, visited, hm) = traversalHelper g (DSL.NoMore, Nothing, [], queue, visited, hm)
traversalHelper _ (_, Just v0, v1:rest, queue, visited, hm) = (DSL.Observe v0 v1, (DSL.Observe v0 v1, Just v0, rest, Q.enqueue v1 queue, HS.insert v1 visited, hm))
