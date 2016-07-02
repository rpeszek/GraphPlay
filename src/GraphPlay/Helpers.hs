{-# LANGUAGE ScopedTypeVariables  #-}

--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module GraphPlay.Helpers (
  second',
  first',
  FoldOptimizer(..)
) where

second' :: (a,b) -> b
second' (_,x) = x

first' :: (a,b) -> a
first' (x,_) = x

--liftSTHelper :: forall s m a b. (Monad m)=> (a -> b) -> m (ST s a) -> m (ST s b)
--liftSTHelper = liftM . liftM

--liftSTPairHelper :: forall s m e x . (Monad m)=> e -> m (ST s x) -> m (ST s (e,x))
--liftSTPairHelper e1 =  liftSTHelper $ (,) e1

data FoldOptimizer m a b = FoldOptimizer {
      optimize :: (a -> m b) -> a -> m b
}
