module FreeDSL.VTraversal (
  VTraverseDslCmds(..)
  , VTraversal
  , VObservation(..)
  , startAt
  , nextVertex
  , put
  , getWithDefault 
) where
  
--import Control.Monad
import Control.Monad.Free (Free(..),liftF)
--import Control.Monad.State (State, execState, modify)
--import qualified PolyGraph.Common.NonBlockingQueue as Q

data VObservation v = Observe {
   on    :: v, 
   neighbor :: v
} | NoMore

data VTraverseDslCmds a v n = 
                    StartAt   v n                     |
                    NextVertex  (VObservation v -> n) |
                    Put  (v, a) n                     |
                    Get  (a, v) ( a -> n)
                               deriving (Functor)

type VTraversal a v = Free (VTraverseDslCmds a v)

startAt :: forall a v .  v -> VTraversal a v ()
startAt v  = liftF (StartAt v ())

nextVertex :: forall a v . VTraversal a v (VObservation v)
nextVertex  = liftF (NextVertex  id)

put :: forall a v .  v -> a -> VTraversal a v ()
put  v a = liftF (Put (v,a) ())

getWithDefault :: forall a v .  a -> v -> VTraversal a v a
getWithDefault defA v = liftF (Get (defA,v) id)
