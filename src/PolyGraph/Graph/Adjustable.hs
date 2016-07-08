module PolyGraph.Graph.Adjustable where

import PolyGraph.Graph

class BuildableSemantics e v where
  defaultEdge :: v -> v -> e

instance BuildableSemantics (v,v) v where
  defaultEdge = (,)
  
class Graph g v e t  => BuildableGraphDataSet g v e t where
  empty :: g

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
addDefaultEdge :: forall g v e t . (BuildableSemantics e v, BuildableGraphDataSet g v e t ) => g -> v -> v -> g
addDefaultEdge g v1 v2 = g ~+ (defaultEdge v1 v2)

(@@+) :: forall g v e t . (BuildableSemantics e v, BuildableGraphDataSet g v e t ) => g -> v -> v -> g
(@@+) = addDefaultEdge
