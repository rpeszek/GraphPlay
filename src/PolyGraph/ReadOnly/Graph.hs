
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
