module PolyGraph.Buildable.DiGBuild where

import PolyGraph.Buildable.GDSBuild
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics)


(@+~>@) :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~>@) = addDefaultEdge

--(:->:)
(^+~>^) :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~>^) s1 s2 g = let v1 = fromString s1 :: v
                      v2 = fromString s2 :: v
                  in  addDefaultEdge v1 v2 g
