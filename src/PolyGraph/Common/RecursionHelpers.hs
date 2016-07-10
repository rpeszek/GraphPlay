
module PolyGraph.Common.RecursionHelpers (
               HashTable,
               memo,
               noReentry,
               handleReentry,
               dumpMemoStore,
               RecursionHandler (..)
               ) where --exports everything, terrible programmer

import Control.Monad
import Control.Monad.ST
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H

data RecursionHandler m a b = RecursionHandler {
      handle :: (a -> m b) -> a -> m b
}

-- memo helpers use ST Monad

type HashTable s k v = C.HashTable s k v

-- passing ST.pure hash allows for better hash sharing
memo :: (Eq a, Hashable a) => HashTable s a b -> (a -> ST s b) -> (a -> ST s b)
memo ht f a = do
   maybe_b <- H.lookup ht a
   b <- case maybe_b of
           Nothing -> f a
           Just _b -> return _b
   case maybe_b of
        Nothing -> H.insert ht a b
        _  -> return ()
   return b

--
-- result function returns f1 (second fn arg) function result on first call and f2 (first fn arg) afterwords
-- this is a re-entry handler
--
handleReentry :: (Eq a, Hashable a) => HashTable s a Bool -> (a -> ST s b) -> (a -> ST s b) -> (a -> ST s b)
handleReentry ht handler f a = do
   maybe_b <- H.lookup ht a
   H.insert ht a True
   case maybe_b of
           Nothing ->
               f a
           Just x  ->
               handler a


noReentry :: (Eq a, Hashable a) => HashTable s a Bool -> (a -> ST s b) -> (a -> ST s b)
noReentry ht = handleReentry ht (\_ -> fail "reentry-detected")
{-
noReentry ht f a = do
   maybe_b <- H.lookup ht a
   H.insert ht a True
   case maybe_b of
           Nothing ->
               f a
           Just x  ->
               fail ("re-entry detected")
-}


dumpMemoStore :: (Eq a, Hashable a) => HashTable s a b -> ST s [(a,b)]
dumpMemoStore h = do
     H.foldM (\list el -> return (el: list)) [] h

-- TESTS ---
-- TMemoExperiments have code that traces

fib :: Int -> ST s Int
fib 0 = return 0
fib 1 = return 1
fib i = do
         f1 <- fib (i-1)
         f2 <- fib (i-2)
         return (f1 + f2)

fibX :: HashTable s Int Int -> Int -> ST s Int
fibX h 0 = return 0
fibX h 1 = return 1
fibX h i = do
         f1 <- memo h (fibX h) $ (i-1)
         f2 <- memo h (fibX h) $ (i-2)
         return (f1 + f2)

runFib :: Int -> ST s Int
runFib i = do
  ht <- H.new:: ST s (HashTable s Int Int)
  f <- fibX ht i
  return f

fibY :: HashTable s Int Bool -> HashTable s Int Int -> Int -> ST s Int
fibY h0 h 0 = return 0
fibY h0 h 1 = return 1
fibY h0 h i = do
           f1 <- ((memo h) . (noReentry h0)) (fibY h0 h) $ (i-1)
           f2 <- ((memo h) . (noReentry h0)) (fibY h0 h) $ (i-2)
           return (f1 + f2)

runFibY :: Int -> ST s Int
runFibY i = do
  ht0 <- H.new :: ST s (HashTable s Int Bool)
  ht <- H.new:: ST s (HashTable s Int Int)
  f <- fibY ht0 ht i
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
