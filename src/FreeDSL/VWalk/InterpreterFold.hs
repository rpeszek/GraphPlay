
module FreeDSL.VWalk.InterpreterFold where

import Control.Monad.State.Strict
import PolyGraph.Common.DslSupport (interpretInM)
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..))
import FreeDSL.VWalk


--  interpretStepM :: c -> f (m a) -> m a

--interpretInM :: forall f a m c. MInterpreterWithCtx c m f => c -> Free f a -> m a
--interpretInM c prog = foldDslProg return (interpretStepM c) prog
{-
-- not useds
interpretWalk :: forall g v e t m r. (Eq v, AdjacencyIndex g v e t, MonadState ([v]) m) =>
                               VWalkDSL v r -> g -> v -> m r
interpretWalk prog g v = interpretInM (g,v) prog

interpretWalk' :: forall g v e t m r. (Eq v, AdjacencyIndex g v e t, Monad m) =>
                               VWalkDSL v r -> g -> v -> StateT [v] m r
interpretWalk' prog g v = interpretInM (g,v) prog
-}

--
-- note this scope narrowing works, MonadState just narrows to StateT when runStateT is used
--
runWalkFull :: forall  g v e t r m . (Eq v, Monad m, AdjacencyIndex g v e t) => 
                               VWalkDSL v r -> g -> v -> m (r, [v])
runWalkFull program g v = runStateT (interpretInM g program) [v] -- ([] is empty initial state)

runWalkState :: forall  g v e t r m . (Eq v, Monad m, AdjacencyIndex g v e t) => 
                               VWalkDSL v r -> g -> v -> m [v]
runWalkState program g  =  liftM snd . runWalkFull program g

runWalk :: forall g v e t r m. (Eq v, Monad m, AdjacencyIndex g v e t) => 
                               VWalkDSL v r -> g -> v -> m r
runWalk program g  =  liftM fst . runWalkFull program g
