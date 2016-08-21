module FreeDSL.VWalk.CoWalk where

import FreeDSL.VWalk
import Control.Comonad.Cofree

import PolyGraph.Common.DslSupport.Pairing
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)

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

mkCoVWalk :: forall g v e t . (Eq v, AdjacencyIndex g v e t) => 
                               g -> v -> CoVWalk v (g, [v])
mkCoVWalk g v = coiter next start 
               where next w = VWalkInt (coNeighbors w) (coWalkTo w) (coHistory w)
                     start = (g, [v])

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
                              g -> VWalk v r -> CoVWalk v a -> r
runPaired _ prog coprog = pair (\_ b -> b) coprog prog

interpretWalk :: forall g v e t r . (Eq v, AdjacencyIndex g v e t) => 
                              VWalk v r -> g -> v -> r
interpretWalk prog g v = runPaired g prog (mkCoVWalk g v)
