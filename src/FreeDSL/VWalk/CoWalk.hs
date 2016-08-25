module FreeDSL.VWalk.CoWalk (
    VWalkInterpreter
   , VWalkCoinstructions(..)
   , buildWalkInstructions
   , interpretWalk
   , VWalkContainer (..)
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


instance Pairing (VWalkCoinstructions v) (VWalkInstructions v) where
  pair f (VWalkCoinstructions a _ _) (GetNeighbors k) = pair f a k
  pair f (VWalkCoinstructions _ c _) (WalkTo x k) = pair f (c x) k
  pair f (VWalkCoinstructions _ _ t) (History k) = pair f t k


class VWalkContainer c v g where
  extractWalk :: c -> (g, [v])
  extendWalk :: c -> (c -> (g, [v])) -> c

replaceWalk :: (VWalkContainer c v g) => c -> (g, [v]) -> c 
replaceWalk c a = extendWalk c (const a) 

instance VWalkContainer (g, [v]) v g where
  extractWalk = id
  extendWalk c f = f c

buildWalkInstructions :: forall g v e t c. (VWalkContainer c v g, Eq v, AdjacencyIndex g v e t) => 
                              g -> c -> VWalkCoinstructions v c
buildWalkInstructions g w = VWalkCoinstructions (coNeighbors g w) (coWalkTo g w) (coHistory g w) 

coNeighbors :: forall g v e t c. (VWalkContainer c v g, Eq v, AdjacencyIndex g v e t) => 
                   g -> c -> ([v], c)
coNeighbors _ c = --todo exception handling
                       let (g, list) = extractWalk c :: (g, [v])
                           choiceVs = neighborsOf g (head list)
                        in (choiceVs, c)

coWalkTo :: forall  g v e t c . (VWalkContainer c v g, Eq v, AdjacencyIndex g v e t) => 
                  g -> c -> v -> (v, c)
coWalkTo _ c v = let (g,vlist) = extractWalk c :: (g, [v])
                        in (v, replaceWalk c (g, v:vlist))

coHistory:: forall  g v e t c. (VWalkContainer c v g, Eq v, AdjacencyIndex g v e t) => 
                  g -> c -> ([v], c)
coHistory _ c = (snd $ (extractWalk c :: (g, [v])), c)


type VWalkInterpreter v r = Cofree (VWalkCoinstructions v) r

buildWalkInterpreter :: forall g v e t . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> VWalkInterpreter v (g, [v])
buildWalkInterpreter g v = coiter (buildWalkInstructions g) (g, [v])

runPaired :: forall a g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              g -> VWalkDSL v r -> VWalkInterpreter v a -> r
runPaired _ prog coprog = pair (\_ b -> b) coprog prog

interpretWalk :: forall g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              VWalkDSL v r -> g -> v -> r
interpretWalk prog g v = runPaired g prog (buildWalkInterpreter g v)
