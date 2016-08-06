
module PolyGraph.Common.NonBlockingQueue.Properties (
    isFifo
    , dequeuesUnlessEmpty
) where
  
import qualified PolyGraph.Common.NonBlockingQueue as Q
import qualified Data.List as L
import qualified Data.Maybe as M

--
-- runQueue (sourceList, instructionList) initialQueue
--
-- instructionList elements:  
-- True = enqueue next element from source list, 
-- False = dequeue
--
-- Runs queue instructions against source and returns
-- Maybe results from all dequeue operations and current queue states.
-- If sourceList is exausted, it just runs remaining dequeue operations
--
runQueue :: ([a],[Bool]) -> Q.SimpleQueue a -> [(Maybe a, Q.SimpleQueue a)]
runQueue (_,      [])           _  = []
runQueue (src,    (False : xs)) q  = 
                              let (x, newQ) = Q.dequeue q
                              in (x, q) : (runQueue (src, xs) newQ)
runQueue ([],     (True : xs))  q  = runQueue ([], xs) q
runQueue ((a:as), (True : xs))  q  = 
                              let newQ = a `Q.enqueue` q
                              in runQueue (as, xs) newQ 

isFifo :: Eq a => ([a],[Bool]) -> Bool
isFifo (source, enqOrDeq) =
          let  results  = runQueue (source, enqOrDeq) Q.emptyQueue
               hitResults = map M.fromJust . L.filter (M.isJust) . map fst $ results
               hitSize = L.length hitResults
          in hitResults == L.take hitSize source

dequeuesUnlessEmpty :: ([a],[Bool]) -> Bool
dequeuesUnlessEmpty (source, enqOrDeq) =
          let results = runQueue (source, enqOrDeq) Q.emptyQueue
           in all (Q.isEmpty . snd) . L.filter (M.isNothing . fst) $ results
