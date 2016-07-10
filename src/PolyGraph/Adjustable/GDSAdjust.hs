module PolyGraph.Adjustable.GDSAdjust where

import PolyGraph.Buildable.GDSBuild

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
