
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Common.Helpers (
  second'
  , first'
  , first
  , second
  , HPair (..)
  , UnorderedHPair (..)
  , HPairLike (..)
  , oneElPair
  , hPairFirst
  , hPairSecond
) where

import Data.Hashable

-- Helper class and types --
class HPairLike a b | a-> b where
  toPair :: a -> (b,b)
  fromPair :: (b,b) -> a

oneElPair :: (Eq v, HPairLike e v) => e -> Bool
oneElPair pair =
        let (v1,v2) = toPair pair
        in v1 == v2

hPairFirst :: (HPairLike e v) => e -> v
hPairFirst e = first' . toPair $ e

hPairSecond :: (HPairLike e v) => e -> v
hPairSecond e = second' . toPair $ e

--
-- helper type for ordered and pairs of the same type (homologous)
-- I need that to make it a Functor
--
newtype HPair a = HPair (a,a) deriving (Eq, Show)

-- HPair instances --
instance forall v . HPairLike (HPair v) v where
  toPair (HPair (a1,a2)) = (a1,a2)
  fromPair = HPair

instance forall v. Hashable v => Hashable (HPair v) where
  hashWithSalt salt (HPair (v1,v2)) = hashWithSalt salt (v1,v2)

instance Functor (HPair) where
    fmap f (HPair (x,y)) = HPair (f x, f y)


-- UnorderdPair used for resolving Graph edges --
newtype UnorderedHPair v = UnorderedHPair (v,v) deriving Show

instance forall v . HPairLike (UnorderedHPair v) v where
  toPair (UnorderedHPair (a1,a2)) = (a1,a2)
  fromPair = UnorderedHPair

instance forall v . (Eq v) => Eq(UnorderedHPair v) where
  UnorderedHPair (a1,a2) == UnorderedHPair (b1,b2) = (a1 == b1 && a2 == b2) || (a1 == b2 && a2 == b1)

instance forall v. Hashable v => Hashable (UnorderedHPair v) where
  hashWithSalt salt (UnorderedHPair (v1,v2)) = (hashWithSalt salt v1) + (hashWithSalt salt v2)

instance Functor (UnorderedHPair) where
    fmap f (UnorderedHPair (x,y)) = UnorderedHPair (f x, f y)



-- helper funtions ---
second :: HPair v -> v
second (HPair (_,x)) = x

first :: HPair v -> v
first (HPair (x,_)) = x

second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x



--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1
