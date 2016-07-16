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

-- | this graph has no edges, it acts in forgetful way if edges are added
newtype Vertices v e = Vertices { getVertices :: [v]} deriving Show

-- | this graph has no isolatedVertices, it acts in forgetful way if isolatedVertex is added
newtype Edges v = Edges { getEdges :: [HPair v]} deriving Show

-- instances Vertices --
instance  forall v e. (Eq v)=> (GraphDataSet (Vertices v e) v e []) where
  isolatedVertices g = getVertices g
  edges g  =  []

instance forall v e. (DiEdgeSemantics e v) => (DiAdjacencyIndex (Vertices v e) v e []) where
   cEdgesOf g ver = []

instance  forall v e. (Eq v, DiEdgeSemantics e v) => (DiGraph (Vertices v e) v e [])

instance  forall v e. (Eq v, EdgeSemantics e v) => (Graph (Vertices v e) v e [])

-- adding edge simply ignores edge and adds vertex
instance  forall v e . (Eq v, EdgeSemantics e v) => BuildableGraphDataSet (Vertices v e) v e [] where

   empty = Vertices []

   g @+ v = Vertices ( v : (getVertices g) )
   g ~+ e =
            let HPair (v1,v2) = resolveEdge e
            in g @+ v1 @+ v2

   union g1 g2 = Vertices ((getVertices g1) ++ (getVertices g2))

instance  forall v e . (Eq v, Eq e, EdgeSemantics e v) => AdjustableGraphDataSet (Vertices v e) v e [] where

   g @\ f = Vertices ( filter f (getVertices g) )

   filterEdges strict g f = g

-- TODO finish this for Edges
