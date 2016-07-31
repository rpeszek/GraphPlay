module PolyGraph.Buildable.DiGraph (
  (@+~>@)
 ,(^+~>^)
 --, toSimpleDiGraph 
)where

--import PolyGraph.Common (toPair)
import PolyGraph.Buildable
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics(..))

-- creates type-checked di-edge
(@+~>@) :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~>@) = addDefaultEdge

-- creates di-edge by deserializing Strings
(^+~>^) :: forall g v e t . (PrettyRead v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~>^) s1 s2 g = let v1 = fromString s1 :: v
                      v2 = fromString s2 :: v
                  in  addDefaultEdge v1 v2 g

toSimpleDiGraph :: forall g v e t . (Eq e, DiEdgeSemantics e v, BuildableGraphDataSet g v e t ) =>  g -> g
toSimpleDiGraph = undefined
