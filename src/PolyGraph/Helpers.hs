
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Helpers (
  second',
  first',
  ToString
) where

second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x


class ToString a where
  toString :: a -> String


instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show


--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1
