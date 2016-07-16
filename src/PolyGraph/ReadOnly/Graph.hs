
module PolyGraph.ReadOnly.Graph (
   EdgeSemantics(..)
   , GraphDataSet(..)
   , Graph (..)
   , defaultVertexCount
   , GMorphism (..)
   --, isValidMorphism
   , pairGMorphism
   , fGMorphism
) where --exports everything, a terrible programmer wrote it

import Data.List (nub, length)
import PolyGraph.Common.Helpers

--
-- e are edges v are vertices, the order of (HPair v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v  where
  resolveEdge      ::  e -> HPair v


class (Eq v, Foldable t)  => GraphDataSet g v e t | g -> t, g -> v, g -> e where
  isolatedVertices ::  g -> t v
  edges            ::  g -> t e

  -- | edge count
  e    ::  g -> Int
  e g  =  length . edges $ g

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t

defaultVertexCount :: forall g v e t. (GraphDataSet g v e t) => (e -> HPair v) -> g -> Int
defaultVertexCount f g =
     let isolatedVCount = length . isolatedVertices $ g
         appendVertices :: e -> [v] -> [v]
         appendVertices e list =
                              let HPair (v1, v2) = f e
                              in  v1 : v2 : list
         nonIsolatedVCount = length . nub $ foldr appendVertices [] (edges g)
     in  isolatedVCount + nonIsolatedVCount


--
data GMorphism v0 e0 v1 e1 = GMorphism {
   vTrans :: v0 -> v1,
   eTrans :: e0 -> e1
}

fGMorphism :: forall v0 v1 f . (Functor f) => (v0 -> v1) -> GMorphism v0 (f v0) v1 (f v1)
fGMorphism fn = GMorphism {
     vTrans = fn,
     eTrans = fmap fn
 }

-- Pair (a,a) is not Funtor in the list sense
pairGMorphism :: forall v0 v1 f . (v0 -> v1) -> GMorphism v0 (v0, v0) v1 (v1, v1)
pairGMorphism fn = GMorphism {
     vTrans = fn,
     eTrans = (\(v1,v2) -> (fn v1, fn v2))
 }

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
