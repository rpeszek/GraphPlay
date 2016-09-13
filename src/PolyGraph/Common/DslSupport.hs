
module PolyGraph.Common.DslSupport (
  IoInterpreter (..)
  , MInterpreterWithCtx (..)
  , foldDslProg
  , interpretInIO
  , interpretInM
) where

import Control.Monad.Free
import PolyGraph.Common.DslSupport.Coproduct

class (Monad m, Functor f) => MInterpreterWithCtx c m f where
  interpretStepM :: c -> f (m a) -> m a

instance (MInterpreterWithCtx c m f, MInterpreterWithCtx c m g) => MInterpreterWithCtx c m (f :+: g) where
  interpretStepM c (InL r) = interpretStepM c r
  interpretStepM c (InR r) = interpretStepM c r

-- TODO not needed special case of the above?
class Functor f => IoInterpreter f where
  interpretStepIO :: f (IO a) -> IO a

instance (IoInterpreter f, IoInterpreter g) => IoInterpreter (f :+: g) where
  interpretStepIO (InL r) = interpretStepIO r
  interpretStepIO (InR r) = interpretStepIO r

foldDslProg :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b 
foldDslProg pur _   (Pure x ) = pur x
foldDslProg pur imp (Free t) = imp (fmap (foldDslProg pur imp) t)

interpretInIO :: forall f a . IoInterpreter f => Free f a -> IO a
interpretInIO prog = foldDslProg return interpretStepIO prog


interpretInM :: forall f a m c. MInterpreterWithCtx c m f => c -> Free f a -> m a
interpretInM c prog = foldDslProg return (interpretStepM c) prog
