{-
  Convenience graphs that can be used to build more complex graphs or to examine
  edge or vertex aspect of polymorphic graph.
  Even simpler than SimpleGraph,  List of vertices and List of pair-edges are defined as graphs
-}
module PolyGraph.Instances.ListGraphs (
  Vertices (..)
  , Edges (..)
) where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import PolyGraph.Common.BuildableCollection
import Data.List (nub, null, lines, words, concat)

-- | this graph has no edges
newtype Vertices v e = Vertices { getVertices :: [v]} deriving Show

-- | this graph has no isolatedVertices
newtype Edges v = Edges { getEdges :: [HPair v]} deriving Show

-- instances --
instance  forall v e. (Eq v)=> (GraphDataSet (Vertices v e) v e []) where
  isolatedVertices g = getVertices g
  edges g  =  []

instance forall v e. (DiEdgeSemantics e v) => (DiAdjacencyIndex (Vertices v e) v e []) where
   cEdgesOf g ver = []

-- TODO continue
