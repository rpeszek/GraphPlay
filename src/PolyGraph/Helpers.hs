
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Helpers (
  second',
  first',
  FromString (..),
  ToString
) where

second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x

class FromString a where
  fromString :: String -> a

class ToString a where
  toString :: a -> String


instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show

instance FromString String where
  fromString = id

instance forall v. (Read v) => FromString v where
  fromString = read


--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1
