module PolyGraph.Graph.PolyBuild where

import PolyGraph.Graph
import PolyGraph.DiGraph (DiEdgeSemantics)
import qualified PolyGraph.Helpers as H

class BuildableEdgeSemantics e v where
  defaultEdge :: v -> v -> e

instance forall v. (Read v) => BuildableEdgeSemantics (v,v) v where
  defaultEdge = (,)

class GraphDataSet g v e t  => BuildableGraphDataSet g v e t where
  empty :: g
  emptyGraph :: g
  emptyGraph = empty
  -- | adds vertex, implemenation decides what to do if same vertex is added twice
  (@+) :: g -> v -> g

  -- | adds edge (and vertices if needed), implementation decides what to do if same edge is added twice, Eq not assumed on e here
  (~+) :: g -> e -> g

  (@++) :: g -> [v] -> g
  g @++ []     = g
  g @++ (v:vs) = g @+ v @++ vs

  (~++) :: g -> [e] -> g
  g ~++ []     = g
  g ~++ (e:es) = g ~+ e ~++ es

class (Eq e, BuildableGraphDataSet g v e t)  => AdjustableGraphDataSet g v e t where
  -- \ vertex induced subgraph
  (@\) :: g -> (v -> Bool) -> g

  -- | edge induced subgraph
  (~\) :: g -> (e -> Bool) -> g

  -- | removes vertex and adjaced edges if v is part of the Graph
  (@-) :: g -> v -> g
  g @- v = g @\ (== v)

  -- | removes edge if one is on the graph (keeps vertices)
  (~-) :: g -> e -> g
  g ~- e = g ~\ (== e)

-- adds edge with default semantics between vertices
addDefaultEdge :: forall g v e t . (BuildableEdgeSemantics e v, BuildableGraphDataSet g v e t ) => v -> v -> g -> g
addDefaultEdge v1 v2 g = g ~+ (defaultEdge v1 v2)

(@+~>@) :: forall g v e t . (BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~>@) = addDefaultEdge

--(:->:)
(^+~>^) :: forall g v e t . (H.FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~>^) s1 s2 g = let v1 = H.fromString s1 :: v
                      v2 = H.fromString s2 :: v
                  in  addDefaultEdge v1 v2 g

(@+~~@) :: forall g v e t . (BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  v -> v -> g -> g
(@+~~@) = addDefaultEdge

(^+~~^) :: forall g v e t . (H.FromString v, BuildableEdgeSemantics e v, EdgeSemantics e v, BuildableGraphDataSet g v e t) =>  String -> String -> g -> g
(^+~~^) s1 s2 g = let v1 = H.fromString s1 :: v
                      v2 = H.fromString s2 :: v
                  in  addDefaultEdge v1 v2 g
