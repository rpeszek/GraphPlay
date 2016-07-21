module PolyGraph.Buildable.Graph where

import PolyGraph.Buildable
import PolyGraph.ReadOnly.Graph

(@+~~@) :: forall g v e t . (BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~~@) = addDefaultEdge

(^+~~^) :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~~^) s1 s2 g = let v1 = fromString s1 :: v
                      v2 = fromString s2 :: v
                  in  addDefaultEdge v1 v2 g
