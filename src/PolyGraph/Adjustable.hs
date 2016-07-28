--
--
module PolyGraph.Adjustable (
  AdjustableGraphDataSet(..)
) where

import PolyGraph.Buildable

class (Eq e, BuildableGraphDataSet g v e t)  => AdjustableGraphDataSet g v e t where
  -- |filterEdges strict graph filterF
  --  strict leaves only vertices induced by edges
  filterEdges :: Bool -> g -> (e -> Bool) -> g

  -- \ vertex induced subgraph
  (\@) :: g -> (v -> Bool) -> g

  (@\) :: (v -> Bool) -> g -> g
  (@\) = flip (\@)

  -- | edge induced subgraph
  (\~) :: g -> (e -> Bool) -> g
  (\~) = filterEdges True

  (~\) :: (e -> Bool) -> g -> g
  (~\) = flip (\~)

  -- | removes vertex and adjaced edges if v is part of the Graph
  (-@) :: g -> v -> g
  g -@ v = g \@ (== v)

  (@-) :: v -> g ->  g
  (@-) = flip (-@)

  -- | removes edge if one is on the graph (keeps vertices)
  (-~) :: g -> e -> g
  g -~ e = filterEdges False g (== e)

  (~-) :: e -> g -> g
  (~-) = flip (-~)
