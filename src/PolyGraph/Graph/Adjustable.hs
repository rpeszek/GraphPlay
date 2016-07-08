module PolyGraph.Graph.Adjustable where

import PolyGraph.Graph  

class Graph g v e t  => BuildableGraphDataSet g v e t where
  empty :: g
  (@+) :: g -> v -> g           -- adds vertex
  (~+) :: g -> e -> g           -- adds edge (and vertices if needed)

  (@++) :: g -> [v] -> g
  g @++ []     = g
  g @++ (v:vs) = g @+ v @++ vs

  (~++) :: g -> [e] -> g
  g ~++ []     = g
  g ~++ (e:es) = g ~+ e ~++ es

class BuildableGraphDataSet g v e t  => AdjustableGraphDataSet g v e t where
  (@-) :: g -> v -> g           -- removes vertex and adjaced edges if v is part of the Graph
  (~-) :: g -> e -> g           -- removes edge if one is on the graph (keeps vertices)
  (@\) :: g -> (v -> Bool) -> g -- vertex induced subgraph
  (+\) :: g -> (e -> Bool) -> g -- edge induced subgraph
