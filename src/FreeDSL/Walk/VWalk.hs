
module FreeDSL.Walk.VWalk where

import Control.Monad
import Control.Monad.Free
import Control.Comonad.Cofree

import PolyGraph.Common.Pairing
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)
import Control.Monad.State (State, execState, evalState, runState, modify, get, put)

import S1_Cstr.E05_Samples (grid)
import qualified Instances.ListGraphs as ListGraphs


data VWalkCmds v r = GetNeighbors ([v] -> r)      |
                     WalkTo v (v -> r)            |
                     History  ([v] -> r) deriving (Functor)

type VWalk v = Free (VWalkCmds v) 

stepWith ::  ([v] -> v) -> VWalk v v
stepWith f = do
    n <- getNeighbors
    walkTo $ f n

getNeighbors :: VWalk v [v]
getNeighbors = liftF (GetNeighbors id)

walkTo :: v -> VWalk v v
walkTo v = liftF (WalkTo v id)

history :: VWalk v [v]
history = liftF (History id)

whereAmI :: VWalk v v
whereAmI = (liftM head) history

-- Comonad interpreter
data VWalkInt v k = VWalkInt {
    getNeighborsI  :: ([v], k)
  , walkToI :: v -> (v, k)
  , historyI :: ([v], k)
  } deriving Functor

type CoVWalk v r = Cofree (VWalkInt v) r

instance Pairing (VWalkInt v) (VWalkCmds v) where
  pair f (VWalkInt a _ _) (GetNeighbors k) = pair f a k
  pair f (VWalkInt _ c _) (WalkTo x k) = pair f (c x) k
  pair f (VWalkInt _ _ t) (History k) = pair f t k

mkCoVWalk :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> CoVWalk v (g, [v])
mkCoVWalk g v = coiter next start 
               where next w = VWalkInt (coNeighbors w) (coWalkTo w) (coHistory w)
                     start = (g, [v])

coNeighbors :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                   (g, [v]) -> ([v], (g, [v]))
coNeighbors (g, list) = --todo exception handling
                       let choiceVs = neighborsOf g (head list)
                        in (choiceVs, (g, list))

coWalkTo :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                  (g, [v]) -> v -> (v, (g, [v]))
coWalkTo (g,vlist) v = (v, (g, v:vlist))


coHistory:: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                  (g, [v]) -> ([v], (g, [v]))
coHistory (g,vlist) = (vlist, (g,vlist))


runPaired :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              g -> VWalk v r -> CoVWalk v a -> r
runPaired g prog coprog = pair (\_ b -> b) coprog prog

interpretWalk :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              VWalk v r -> g -> v -> r
interpretWalk prog g v = runPaired g prog (mkCoVWalk g v)


-- regular interpreter
interpretWalkStd :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> VWalk v r -> State ([v]) r
interpretWalkStd g v (Free (GetNeighbors nF)) =
       do
         let choiceVs = neighborsOf g v
         if null choiceVs 
         then fail "out of cheese error"
         else interpretWalkStd g v (nF choiceVs)

interpretWalkStd g v (Free (WalkTo choiceV nF)) =
       do
         modify ((:) choiceV)
         interpretWalkStd g choiceV (nF choiceV)

interpretWalkStd g v (Free (History nF)) = 
       do
         path <- get
         interpretWalkStd g v (nF path) 
interpretWalkStd _ _ (Pure r) = return r

runWalkStdFull :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> (r, [v])
runWalkStdFull program g v = runState (interpretWalkStd g v program) [v]


runWalkStdState :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> [v]
runWalkStdState program g v = snd $ runWalkStdFull program g v
  
runWalkStd :: forall a b g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                               VWalk v r -> g -> v -> r
runWalkStd program g v = fst $ runWalkStdFull program g v
