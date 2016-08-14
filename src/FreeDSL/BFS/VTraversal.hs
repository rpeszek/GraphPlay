module FreeDSL.BFS.VTraversal (
  VTraverseDslCmds(..)
  , VTraversal
  , VObservation(..)
  -- low level API
  , rootAt
  , nextObservation
  , currentObservation
  , annotateAt
  , getAnnotationAt
  , getWithDefault
  -- easy API
  , startWithAnnotation
  , rootWithAnnotation
  , nextVertex
  , annotate
  , adjustAnnotation
  , getAnnotation
  , appendAnnotation
  , currentVertex
  -- other versions
  , nextAsVertexPair
) where
  
--import Control.Monad
import Control.Monad.Free (Free(..),liftF)
--import Control.Monad.State (State, execState, modify)
--import qualified PolyGraph.Common.NonBlockingQueue as Q

data VObservation v = Observe {
   on    :: v, 
   neighbor :: v
} | NoMore deriving Show

data VTraverseDslCmds a v n = 
                    StartAt   v n                     |
                    NextVObs  (VObservation v -> n)   |
                    CurrentVObs (VObservation v -> n) |
                    Put  (v, a) n                     |
                    Get  v ( Maybe a -> n)
                               deriving (Functor)

type VTraversal a v = Free (VTraverseDslCmds a v)

rootAt :: forall a v .  v -> VTraversal a v ()
rootAt v  = liftF (StartAt v ())

nextObservation :: forall a v . VTraversal a v (VObservation v)
nextObservation  = liftF (NextVObs  id)

nextAsVertexPair :: forall a v . VTraversal a v (Maybe (v,v))
nextAsVertexPair  = nextObservation >>=               
              (\obs -> case obs of
                           NoMore -> return Nothing
                           Observe v1 v2 -> return $ Just (v1, v2)
              )


currentObservation :: forall a v . VTraversal a v (VObservation v)
currentObservation  = liftF (CurrentVObs  id)

annotateAt :: forall a v .  v -> a -> VTraversal a v ()
annotateAt  v a = liftF (Put (v,a) ())

getAnnotationAt :: forall a v .   v -> VTraversal a v (Maybe a)
getAnnotationAt v = liftF (Get v id)

getWithDefault :: forall a v .  a -> v -> VTraversal a v a
getWithDefault defA v = getAnnotationAt v >>= return . (maybe defA id) 

rootWithAnnotation :: forall a v .  v -> a -> VTraversal a v ()
rootWithAnnotation v a = rootAt v >> annotateAt v a 

startWithAnnotation :: forall a v .  v -> a -> VTraversal a v (VObservation v)
startWithAnnotation v a = rootWithAnnotation v a >> nextObservation

nextVertex :: forall a v . VTraversal a v (Maybe v)
nextVertex  = nextObservation >>= 
              (\obs -> case obs of
                           NoMore -> return Nothing
                           Observe _ v2 -> return $ Just v2
              )

currentVertex :: forall a v . VTraversal a v (Maybe v)
currentVertex = currentObservation >>= 
              (\obs -> case obs of
                           NoMore -> return Nothing
                           Observe _ v2 -> return $ Just v2
              )

annotate :: forall a v . a -> VTraversal a v ()
annotate a = currentObservation >>= 
             (\obs -> case obs of
                          NoMore -> return ()
                          Observe _ v2 -> annotateAt v2 a
             )

annotateR :: forall a v . a -> VTraversal a v (Maybe a)
annotateR a = currentObservation >>= 
             (\obs -> case obs of
                    NoMore -> return Nothing
                    Observe _ v2 -> do 
                                      annotateAt v2 a
                                      return $ Just a
             )

getAnnotation :: forall a v . VTraversal a v (Maybe a)
getAnnotation = currentObservation >>= 
          (\obs -> case obs of
                       NoMore -> return Nothing
                       Observe v1 _ -> getAnnotationAt v1
          )

adjustAnnotation :: forall a v . (a -> a) ->  VTraversal a v (Maybe a)
adjustAnnotation f = getAnnotation >>= maybe (return Nothing) (annotateR . f)

appendAnnotation :: forall a v . Monoid a => a -> VTraversal a v (Maybe a)
appendAnnotation a = adjustAnnotation (mappend a)
