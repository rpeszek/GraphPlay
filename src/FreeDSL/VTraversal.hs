module FreeDSL.VTraversal (
  VTraverseDslCmds(..)
  , VTraversal
  , VObservation(..)
  -- low level API
  , rootAt
  , nextObservation
  , currentObservation
  , put
  , get
  , getWithDefault
  -- easy API
  , startWithLabel
  , rootWithLabel
  , nextVertex
  , label
  , adjustLabel
  , getLabel
  , appendLabel
  , currentVertex
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

currentObservation :: forall a v . VTraversal a v (VObservation v)
currentObservation  = liftF (CurrentVObs  id)

put :: forall a v .  v -> a -> VTraversal a v ()
put  v a = liftF (Put (v,a) ())

get :: forall a v .   v -> VTraversal a v (Maybe a)
get v = liftF (Get v id)

getWithDefault :: forall a v .  a -> v -> VTraversal a v a
getWithDefault defA v = get v >>= return . (maybe defA id) 

rootWithLabel :: forall a v .  v -> a -> VTraversal a v ()
rootWithLabel v a = rootAt v >> put v a 

startWithLabel :: forall a v .  v -> a -> VTraversal a v (VObservation v)
startWithLabel v a = rootWithLabel v a >> nextObservation

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

label :: forall a v . a -> VTraversal a v ()
label a = currentObservation >>= 
             (\obs -> case obs of
                          NoMore -> return ()
                          Observe _ v2 -> put v2 a
             )

labelR :: forall a v . a -> VTraversal a v (Maybe a)
labelR a = currentObservation >>= 
             (\obs -> case obs of
                    NoMore -> return Nothing
                    Observe _ v2 -> do 
                                      put v2 a
                                      return $ Just a
             )

getLabel :: forall a v . VTraversal a v (Maybe a)
getLabel = currentObservation >>= 
          (\obs -> case obs of
                       NoMore -> return Nothing
                       Observe v1 _ -> get v1
          )

adjustLabel :: forall a v . (a -> a) ->  VTraversal a v (Maybe a)
adjustLabel f = getLabel >>= maybe (return Nothing) (labelR . f)

            
                       
--return . (maybe Nothing (Just . f))

appendLabel :: forall a v . Monoid a => a -> VTraversal a v (Maybe a)
appendLabel a = getLabel >>= return . (maybe Nothing (Just . mappend a))
