module FreeDSL.VWalk.CoWalk (
    VWalkInterpreter
   , VWalkCoinstructions(..)
   , buildWalkInstructions
   , interpretWalk
) where

import FreeDSL.VWalk
import Control.Comonad.Cofree

import PolyGraph.Common.DslSupport.Pairing
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)

-- Comonad interpreter
data VWalkCoinstructions v k = VWalkCoinstructions {
    getNeighborsCoI  :: ([v], k)
  , walkToCoI :: v -> (v, k)
  , historyCoI :: ([v], k)
  } deriving Functor

type VWalkInterpreter v r = Cofree (VWalkCoinstructions v) r

instance Pairing (VWalkCoinstructions v) (VWalkInstructions v) where
  pair f (VWalkCoinstructions a _ _) (GetNeighbors k) = pair f a k
  pair f (VWalkCoinstructions _ c _) (WalkTo x k) = pair f (c x) k
  pair f (VWalkCoinstructions _ _ t) (History k) = pair f t k

buildWalkInterpreter :: forall g v e t . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> VWalkInterpreter v (g, [v])
buildWalkInterpreter g v = coiter buildWalkInstructions (g, [v])

buildWalkInstructions :: forall g v e t . (Eq v, AdjacencyIndex g v e t) => 
                               (g, [v]) -> VWalkCoinstructions v (g, [v])
buildWalkInstructions w = VWalkCoinstructions (coNeighbors w) (coWalkTo w) (coHistory w) 

coNeighbors :: forall g v e t . (Eq v, AdjacencyIndex g v e t) => 
                   (g, [v]) -> ([v], (g, [v]))
coNeighbors (g, list) = --todo exception handling
                       let choiceVs = neighborsOf g (head list)
                        in (choiceVs, (g, list))

coWalkTo :: forall  g v e t  . (Eq v, AdjacencyIndex g v e t) => 
                  (g, [v]) -> v -> (v, (g, [v]))
coWalkTo (g,vlist) v = (v, (g, v:vlist))


coHistory:: forall  g v e t . (Eq v, AdjacencyIndex g v e t) => 
                  (g, [v]) -> ([v], (g, [v]))
coHistory (g,vlist) = (vlist, (g,vlist))


runPaired :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              g -> VWalkDSL v r -> VWalkInterpreter v a -> r
runPaired _ prog coprog = pair (\_ b -> b) coprog prog

interpretWalk :: forall g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              VWalkDSL v r -> g -> v -> r
interpretWalk prog g v = runPaired g prog (buildWalkInterpreter g v)
