{-
 Simple non-blocking queue
-}

module PolyGraph.Common.NonBlockingQueue (
  SimpleQueue
  , enqueue
  , dequeue
  , emptyQueue
  , isEmpty
) where

import qualified Data.List as L

data SimpleQueue a = SimpleQueue [a] [a]

emptyQueue :: SimpleQueue a
emptyQueue = SimpleQueue [] []

isEmpty :: SimpleQueue a -> Bool
isEmpty (SimpleQueue [] []) = True
isEmpty _ = False

enqueue :: a -> SimpleQueue a -> SimpleQueue a
enqueue a (SimpleQueue inL outL) = SimpleQueue (a : inL) outL

dequeue :: SimpleQueue a -> (Maybe a, SimpleQueue a)
dequeue  (SimpleQueue []  [])  = (Nothing, SimpleQueue [] [])
dequeue  (SimpleQueue inL [])  = dequeue $ SimpleQueue [] (L.reverse inL)
dequeue  (SimpleQueue inL (out : outRest))  = (Just out, SimpleQueue inL outRest)
