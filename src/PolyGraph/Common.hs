
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--
module PolyGraph.Common (
  first
  , second
  , OPair (..)
  , UOPair (..)
  , PairLike (..)
  , oneElementPair
  , GraphApp
  , liftOut
  , runApp
  , trace
  , withTrace
  , Trace
  , fstOrSnd
) where

import Data.Hashable (Hashable, hashWithSalt)
import qualified Control.Monad.Writer as W
import qualified Control.Monad.Trans.Class as MTL

-- tranformer used if tracing is needed --
type GraphApp m  = W.WriterT Trace m
type Trace = [String]

trace :: (Monad m, Show a) => String -> a -> GraphApp m ()
trace s a = W.tell [s ++ show a]

withTrace :: (Monad m, Show a) => String -> a -> GraphApp m a
withTrace s a = W.tell [s ++ show a] >> return a

liftOut :: Monad m => m a -> GraphApp m a
liftOut = MTL.lift

runApp ::  GraphApp m a -> m (a, Trace)
runApp app  = W.runWriterT app

-- end App transformer stack --

-- Helper funcitons
fstOrSnd ::  a -> a -> Bool -> a
fstOrSnd a1 a2 b = if b then a1 else a2

-- Helper class and types --
class PairLike a b | a-> b where
  toPair :: a -> (b,b)
  fromPair :: (b,b) -> a

oneElementPair :: (Eq v, PairLike e v) => e -> Bool
oneElementPair pair =
        let (v1,v2) = toPair pair
        in v1 == v2

first :: (PairLike e v) => e -> v
first e = fst . toPair $ e

second :: (PairLike e v) => e -> v
second e = snd . toPair $ e

--
-- Type used to resolve directed edges.  Represents ordered 2-element list.
-- These is more than a superficial diffrence from the (,) type, this one is
-- more of a collection type as Functor (and Monad/Applicative when I get to it)
--
newtype OPair a = OPair (a,a) deriving (Eq, Show, Read)

-- OPair instances --
instance forall v . PairLike (OPair v) v where
  toPair (OPair (a1,a2)) = (a1,a2)
  fromPair = OPair

instance forall v. Hashable v => Hashable (OPair v) where
  hashWithSalt salt (OPair (v1,v2)) = hashWithSalt salt (v1,v2)

instance Functor (OPair) where
    fmap f (OPair (x,y)) = OPair (f x, f y)

--
-- UnorderdPair used for resolving Graph edges
-- Represents 2 element set
--
newtype UOPair v = UOPair (v,v) deriving (Show, Read)

instance forall v . PairLike (UOPair v) v where
  toPair (UOPair (a1,a2)) = (a1,a2)
  fromPair = UOPair

instance forall v . (Eq v) => Eq(UOPair v) where
  UOPair (a1,a2) == UOPair (b1,b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

instance forall v. Hashable v => Hashable (UOPair v) where
  hashWithSalt salt (UOPair (v1,v2)) = (hashWithSalt salt v1) + (hashWithSalt salt v2)

instance Functor (UOPair) where
    fmap f (UOPair (x,y)) = UOPair (f x, f y)




--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1
