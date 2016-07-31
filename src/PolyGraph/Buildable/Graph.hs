module PolyGraph.Buildable.Graph (
    (@+~~@)
  , (^+~~^)
  , toSimpleGraph
) where

import PolyGraph.Common (UOPair(..), toPair)
import PolyGraph.Buildable
import qualified PolyGraph.ReadOnly as Base
import PolyGraph.ReadOnly.Graph (EdgeSemantics (..))
import Data.List (nub)
import Data.Foldable (toList)

(@+~~@) :: forall g v e t . (BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~~@) = addDefaultEdge

(^+~~^) :: forall g v e t . (PrettyRead v, BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~~^) s1 s2 g = let v1 = fromString s1 :: v
                      v2 = fromString s2 :: v
                  in  addDefaultEdge v1 v2 g



toSimpleGraph :: forall g v e t . (EdgeSemantics e v, BuildableGraphDataSet g v e t ) =>  g -> g
toSimpleGraph g =  
                let  toEdgeHelper :: e -> EdgeHelper e v
                     toEdgeHelper e = EdgeHelper (resolveEdge e) e
                     notALoop :: EdgeHelper e v -> Bool
                     notALoop eh = let UOPair(v1,v2) = vPair eh in v1 /= v2
                     withoutIsolatedVerts = fromEdgeList . map(edge) . (filter notALoop) . nub . map(toEdgeHelper) . toList . Base.edges $ g
                in withoutIsolatedVerts ++@ (toList . Base.isolatedVertices $ g)


data EdgeHelper e v= EdgeHelper {
   vPair:: UOPair v,
   edge :: e
}

instance Eq v => Eq(EdgeHelper e v) where
   nh1 == nh2 = vPair nh1 == vPair nh2
