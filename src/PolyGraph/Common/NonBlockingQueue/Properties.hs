
module PolyGraph.Common.NonBlockingQueue.Properties (
    isFifo
    , dequeuesUnlessEmpty
    , QueueInstruction(..)
) where
  
import qualified PolyGraph.Common as Common
import qualified PolyGraph.Common.NonBlockingQueue as Q
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Test.QuickCheck as Property

data QueueInstruction = Enqueue | Dequeue deriving (Eq, Show, Read)

instance Property.Arbitrary QueueInstruction where
   arbitrary = (Property.arbitrary :: Property.Gen Bool) >>= (return . (Common.fstOrSnd Enqueue Dequeue))

--
-- Properties which make for a valid NonBlocking Queue
--
isFifo :: Eq a => ([a],[QueueInstruction]) -> Bool
isFifo (source, instructions) =
          let  results  = runQueue (source, instructions) Q.emptyQueue
               hitResults = map M.fromJust . L.filter (M.isJust) . map fst $ results
               hitSize = L.length hitResults
          in hitResults == L.take hitSize source

dequeuesUnlessEmpty :: ([a],[QueueInstruction]) -> Bool
dequeuesUnlessEmpty (source, instructions) =
          let results = runQueue (source, instructions) Q.emptyQueue
           in all (Q.isEmpty . snd) . L.filter (M.isNothing . fst) $ results


--
-- runQueue (sourceList, instructionList) initialQueue
--
-- instructionList elements:  
-- True = enqueue next element from source list, 
-- False = dequeue
--
-- Runs queue instructions against source and returns
-- Maybe results from all dequeue operations and current queue states.
-- If sourceList is exhausted, it just runs moreInst dequeue operations
--
runQueue :: ([a],[QueueInstruction]) -> Q.SimpleQueue a -> [(Maybe a, Q.SimpleQueue a)]
runQueue (_,           []                  ) _  = []
runQueue (src,         (Dequeue : moreInst)) q  =
                              let (x, newQ) = Q.dequeue q
                              in (x, q) : (runQueue (src, moreInst) newQ)
runQueue ([],          (Enqueue : moreInst))  q  = runQueue ([], moreInst) q
runQueue ((a:moreSrc), (Enqueue : moreInst))  q  =
                              let newQ = a `Q.enqueue` q
                              in runQueue (moreSrc, moreInst) newQ
