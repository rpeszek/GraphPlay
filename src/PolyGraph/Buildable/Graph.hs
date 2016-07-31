module PolyGraph.Buildable.Graph (
    (@+~~@)
  , (^+~~^)
  , toSimpleGraph
) where

import PolyGraph.Common (toPair)
import PolyGraph.Buildable
import PolyGraph.ReadOnly.Graph (EdgeSemantics (..))

(@+~~@) :: forall g v e t . (BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~~@) = addDefaultEdge

(^+~~^) :: forall g v e t . (PrettyRead v, BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~~^) s1 s2 g = let v1 = fromString s1 :: v
                      v2 = fromString s2 :: v
                  in  addDefaultEdge v1 v2 g

toSimpleGraph :: forall g v e t . (Eq e, EdgeSemantics e v, BuildableGraphDataSet g v e t ) =>  g -> g
toSimpleGraph = toSimpleGraphHelper $ toPair .resolveEdge
