module FreeDSL.VWalk.InterpreterStd where

import Control.Monad.Free
import FreeDSL.VWalk
import Control.Monad.State (State,  runState, modify, get)
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)

-- regular interpreter
interpretWalk :: forall  g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> VWalk v r -> State ([v]) r
interpretWalk g v (Free (GetNeighbors nF)) =
       do
         let choiceVs = neighborsOf g v
         if null choiceVs 
         then fail "out of cheese error"
         else interpretWalk g v (nF choiceVs)

interpretWalk g _ (Free (WalkTo choiceV nF)) =
       do
         modify ((:) choiceV)
         interpretWalk g choiceV (nF choiceV)

interpretWalk g v (Free (History nF)) = 
       do
         path <- get
         interpretWalk g v (nF path) 
interpretWalk _ _ (Pure r) = return r

runWalkFull :: forall  g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> (r, [v])
runWalkFull program g v = runState (interpretWalk g v program) [v]


runWalkState :: forall  g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> [v]
runWalkState program g v = snd $ runWalkFull program g v
  
runWalk :: forall  g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> r
runWalk program g v = fst $ runWalkFull program g v
