--
--
module PolyGraph.ReadOnly.Graph (
   EdgeSemantics(..)
   , Graph
   , AdjacencyIndex(..)
   , neighbor
   , neighborsOf
) where --exports everything, a terrible programmer wrote it

import PolyGraph.ReadOnly (GraphDataSet)
import PolyGraph.Common (UOPair(..))
import Data.List (nub)
import Data.Foldable (toList)

--
-- e are edges v are vertices, the order of (OPair v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v  where
  resolveEdge      ::  e -> UOPair v

instance forall v . (Eq v) => (EdgeSemantics (UOPair v) v) where
  resolveEdge e = e

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t

class (Traversable t, EdgeSemantics e v)  => AdjacencyIndex g v e t | g -> t, g -> v, g -> e where
  edgesOf   ::  g -> v -> t e   -- return a list of adjacent edges, empty if not a valid vertex or isolated vertex


neighbor :: forall e v . (Eq v, EdgeSemantics e v) => v -> e -> v
neighbor v e = 
            let UOPair (v1, v2) = resolveEdge e
            in if v1 == v then v2 else v1

neighborsOf :: forall g v e t . (AdjacencyIndex g v e t, Eq v) => g -> v -> [v]
neighborsOf g v = 
           let eList = edgesOf g v :: t e
               neighbors = fmap (neighbor v) eList
           in  nub . toList $ neighbors -- foldr (\UPair (v1 v2) ->  )
