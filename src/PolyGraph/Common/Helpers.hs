
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Common.Helpers (
  second',
  first',
  first,
  second,
  HPair (..)
) where

import Data.Hashable

-- helper type for both ordered and un-ordered pairs of the same type (homologous)
-- I need that to make it a Functor
newtype HPair a = HPair (a,a) deriving (Eq, Show)

instance forall v. Hashable v => Hashable (HPair v) where
  hashWithSalt salt (HPair (v1,v2)) = hashWithSalt salt (v1,v2)

instance Functor (HPair) where
    fmap f (HPair (x,y)) = HPair (f x, f y)


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
