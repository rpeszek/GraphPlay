
module PolyGraph.Common.RecursionHelpers (
               HashTable,
               memo,
               noReentry,
               handleReentry,
               dumpMemoStore,
               RecursionHandler (..)
               ) where --exports everything, terrible programmer

import Control.Monad (forM)
import qualified Control.Monad.ST as St
import Data.Hashable (Hashable)
import qualified Data.HashTable.ST.Cuckoo as C
import qualified Data.HashTable.Class as H

data RecursionHandler m a b = RecursionHandler {
      handle :: (a -> m b) -> a -> m b
}

-- memo helpers use ST Monad

type HashTable s k v = C.HashTable s k v

-- passing ST.pure hash allows for better hash sharing
memo :: (Eq a, Hashable a) => HashTable s a b -> (a -> St.ST s b) -> (a -> St.ST s b)
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
handleReentry :: (Eq a, Hashable a) => HashTable s a Bool -> (a -> St.ST s b) -> (a -> St.ST s b) -> (a -> St.ST s b)
handleReentry ht handler f a = do
   maybe_b <- H.lookup ht a
   H.insert ht a True
   case maybe_b of
           Nothing ->
               f a
           Just _  ->
               handler a


noReentry :: (Eq a, Hashable a) => HashTable s a Bool -> (a -> St.ST s b) -> (a -> St.ST s b)
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


dumpMemoStore :: (Eq a, Hashable a) => HashTable s a b -> St.ST s [(a,b)]
dumpMemoStore h = do
     H.foldM (\list el -> return (el: list)) [] h

-- TESTS ---
-- TMemoExperiments have code that traces

_fib :: Int -> St.ST s Int
_fib 0 = return 0
_fib 1 = return 1
_fib i = do
         f1 <- _fib (i-1)
         f2 <- _fib (i-2)
         return (f1 + f2)

_fibX :: HashTable s Int Int -> Int -> St.ST s Int
_fibX _ 0 = return 0
_fibX _ 1 = return 1
_fibX h i = do
         f1 <- memo h (_fibX h) $ (i-1)
         f2 <- memo h (_fibX h) $ (i-2)
         return (f1 + f2)

_runFib :: Int -> St.ST s Int
_runFib i = do
  ht <- H.new:: St.ST s (HashTable s Int Int)
  f <- _fibX ht i
  return f

_fibY :: HashTable s Int Bool -> HashTable s Int Int -> Int -> St.ST s Int
_fibY _  _ 0 = return 0
_fibY _  _ 1 = return 1
_fibY h0 h i = do
           f1 <- ((memo h) . (noReentry h0)) (_fibY h0 h) $ (i-1)
           f2 <- ((memo h) . (noReentry h0)) (_fibY h0 h) $ (i-2)
           return (f1 + f2)

_runFibY :: Int -> St.ST s Int
_runFibY i = do
  ht0 <- H.new :: St.ST s (HashTable s Int Bool)
  ht <- H.new:: St.ST s (HashTable s Int Int)
  f <- _fibY ht0 ht i
  return f

{-
_regFibIO  n = St.stToIO $ _fib n
_fastFibIO n = St.stToIO (_runFib n)
_regFib n = St.runST $ _fib n
_fastFib n = St.runST (_runFib n)
-}

_compareFibs :: Int -> IO ([(Int, Int)])
_compareFibs n = St.stToIO ( do
     forM [1..n] (\i -> do
                     slow <- _fib i
                     fast <- _runFib i
                     return (slow, fast)
                  ))

-- runST cannot be run compaints about purity loss
-- can be used with stToIO only
