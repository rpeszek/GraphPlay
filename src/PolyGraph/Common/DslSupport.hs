
module PolyGraph.Common.DslSupport (
  IoInterpreter (..)
  , MInterpreterWithCtx (..)
  , foldDslProg
  , execInIO
  , execInM
) where

import Control.Monad.Free
import PolyGraph.Common.DslSupport.Coproduct

class (Monad m, Functor f) => MInterpreterWithCtx c m f where
  interpretM :: c -> f (m a) -> m a

instance (MInterpreterWithCtx c m f, MInterpreterWithCtx c m g) => MInterpreterWithCtx c m (f :+: g) where
  interpretM c (InL r) = interpretM c r
  interpretM c (InR r) = interpretM c r

-- TODO not needed special case of the above?
class Functor f => IoInterpreter f where
  interpretInIO :: f (IO a) -> IO a

instance (IoInterpreter f, IoInterpreter g) => IoInterpreter (f :+: g) where
  interpretInIO (InL r) = interpretInIO r
  interpretInIO (InR r) = interpretInIO r

foldDslProg :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b 
foldDslProg pur _   (Pure x ) = pur x
foldDslProg pur imp (Free t) = imp (fmap (foldDslProg pur imp) t)

execInIO :: forall f a . IoInterpreter f => Free f a -> IO a
execInIO prog = foldDslProg return interpretInIO prog


execInM :: forall f a m c. MInterpreterWithCtx c m f => c -> Free f a -> m a
execInM c prog = foldDslProg return (interpretM c) prog
