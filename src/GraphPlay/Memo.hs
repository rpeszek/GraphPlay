{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}

module GraphPlay.Memo (
               HashTable,
               memo,
               dumpMemoStore
               ) where --exports everything, terrible programmer

import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H


-- memo helpers use ST Monad

type HashTable s k v = C.HashTable s k v

-- passing ST.pure hash allows for better hash sharing
memo :: (Eq a, Hashable a) => ST s (HashTable s a b) -> (a -> ST s b) -> (a -> ST s b)
memo h f a = do
   ht <- h
   maybe_b <- H.lookup ht a
   b <- case maybe_b of
           Nothing -> f a
           Just _b -> return _b
   case maybe_b of
        Nothing -> H.insert ht a b
        _  -> return ()
   return b

dumpMemoStore :: (Eq a, Hashable a) => ST s (HashTable s a b) -> ST s [(a,b)]
dumpMemoStore h = do
     ht <- h
     H.foldM (\list el -> return (el: list)) [] ht

-- TESTS ---
-- TMemoExperiments have code that traces

fib :: Int -> ST s Int
fib 0 = return 0
fib 1 = return 1
fib i = do
         f1 <- fib (i-1)
         f2 <- fib (i-2)
         return (f1 + f2)

fibX :: ST s (HashTable s Int Int) -> Int -> ST s Int
fibX h 0 = return 0
fibX h 1 = return 1
fibX h i = do
         f1 <- memo h (fibX h) $ (i-1)
         f2 <- memo h (fibX h) $ (i-2)
         return (f1 + f2)

runFib :: Int -> ST s Int
runFib i = do
  ht <- H.new:: ST s (HashTable s Int Int)
  f <- fibX (return ht) i
  return f

regFibIO  n = stToIO $ fib n
fastFibIO n = stToIO (runFib n)
regFib n = runST $ fib n
fastFib n = runST (runFib n)

compareFibs :: Int -> IO ([(Int, Int)])
compareFibs n = stToIO ( do
     forM [1..n] (\i -> do
                     slow <- fib i
                     fast <- runFib i
                     return (slow, fast)
                  ))

-- runST cannot be run compaints about purity loss
-- can be used with stToIO only
