
module PolyGraph.ReadOnly.Graph (
   EdgeSemantics(..)
   , Graph (..)
   --, isValidMorphism
) where --exports everything, a terrible programmer wrote it

import PolyGraph.ReadOnly
import PolyGraph.Common (UOPair)

--
-- e are edges v are vertices, the order of (OPair v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v  where
  resolveEdge      ::  e -> UOPair v

instance forall v . (Eq v) => (EdgeSemantics (UOPair v) v) where
  resolveEdge e = e

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t

-- TODO provide edge-vertex consistency check
isValidGraph :: forall g v e t . Graph g v e t => g -> Bool
isValidGraph = undefined


-- TODO implement check
-- to be valid eTrans and resolveEdge needs to commute with the vTrans
isValidMorphism :: forall g v0 e0 t v1 e1 . (GraphDataSet g v0 e0 t, EdgeSemantics e0 v0, EdgeSemantics e1 v1) =>
                               g -> GMorphism v0 e0 v1 e1 -> Bool
isValidMorphism = undefined

{-
-- not needed, use functors directly with fGMorphism
class FunctorialEdgeSemantics e where
   liftToEdge :: (v0 -> v1) -> e v0 -> e v1
   toMorphism :: (v0 -> v1) -> GMorphism v0 (e v0) v1 (e v1)
   toMorphism f = GMorphism {
        vTrans = f,
        eTrans = liftToEdge f
    }
-}
