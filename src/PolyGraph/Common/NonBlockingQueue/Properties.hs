
module PolyGraph.Common.NonBlockingQueue.Properties (
    isFifo
    , dequeuesUnlessEmpty
    , QueueInstruction(..)
) where

import Control.Monad (liftM)
import qualified PolyGraph.Common.NonBlockingQueue as Q
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Property

data QueueInstruction a = Enqueue a | Dequeue deriving (Eq, Show, Read)

instance Property.Arbitrary a => Property.Arbitrary (QueueInstruction a) where
   arbitrary = do 
      enqueue <- Property.arbitrary
      if enqueue
      then liftM Enqueue Property.arbitrary
      else return Dequeue

--
-- Properties which make for a valid NonBlocking Queue
--
isFifo :: Eq a => [QueueInstruction a] -> Bool
isFifo instructions =
          let  input = map (\(Enqueue a) -> a) . L.filter (/= Dequeue) $ instructions
               results  = runQueue instructions Q.emptyQueue
               hitResults = map M.fromJust . L.filter (M.isJust) . map fst $ results
               hitSize = L.length hitResults
          in hitResults == L.take hitSize input

dequeuesUnlessEmpty :: [QueueInstruction a] -> Bool
dequeuesUnlessEmpty instructions =
          let results = runQueue instructions Q.emptyQueue
           in all (Q.isEmpty . snd) . L.filter (M.isNothing . fst) $ results


--
-- runQueue instructionList initialQueue
--
-- Runs queue instructions and returns
-- Maybe results from all dequeue operations and current queue states.
--
runQueue :: ([QueueInstruction a]) -> Q.SimpleQueue a -> [(Maybe a, Q.SimpleQueue a)]
runQueue []                   _  = []
runQueue (Dequeue : moreInst) q  =
                              let (x, newQ) = Q.dequeue q
                              in (x, q) : (runQueue moreInst newQ)
runQueue (Enqueue a: moreInst) q  =
                              let newQ = a `Q.enqueue` q
                              in runQueue moreInst newQ
