
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Common.Helpers (
  pairSecond
  , pairFirst
  , first
  , second
  , OPair (..)
  , UOPair (..)
  , PairLike (..)
  , oneElementPair
) where

import Data.Hashable

-- Helper class and types --
class PairLike a b | a-> b where
  toPair :: a -> (b,b)
  fromPair :: (b,b) -> a

oneElementPair :: (Eq v, PairLike e v) => e -> Bool
oneElementPair pair =
        let (v1,v2) = toPair pair
        in v1 == v2

first :: (PairLike e v) => e -> v
first e = pairFirst . toPair $ e

second :: (PairLike e v) => e -> v
second e = pairSecond . toPair $ e

--
-- helper type for ordered and pairs of the same type (homologous)
-- I need that to make it a Functor
--
newtype OPair a = OPair (a,a) deriving (Eq, Show)

-- OPair instances --
instance forall v . PairLike (OPair v) v where
  toPair (OPair (a1,a2)) = (a1,a2)
  fromPair = OPair

instance forall v. Hashable v => Hashable (OPair v) where
  hashWithSalt salt (OPair (v1,v2)) = hashWithSalt salt (v1,v2)

instance Functor (OPair) where
    fmap f (OPair (x,y)) = OPair (f x, f y)


-- UnorderdPair used for resolving Graph edges --
newtype UOPair v = UOPair (v,v) deriving Show

instance forall v . PairLike (UOPair v) v where
  toPair (UOPair (a1,a2)) = (a1,a2)
  fromPair = UOPair

instance forall v . (Eq v) => Eq(UOPair v) where
  UOPair (a1,a2) == UOPair (b1,b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

instance forall v. Hashable v => Hashable (UOPair v) where
  hashWithSalt salt (UOPair (v1,v2)) = (hashWithSalt salt v1) + (hashWithSalt salt v2)

instance Functor (UOPair) where
    fmap f (UOPair (x,y)) = UOPair (f x, f y)



pairSecond :: (a,b) -> b
pairSecond (_,x) = x

pairFirst :: (a,b) -> a
pairFirst (x,_) = x



--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1
