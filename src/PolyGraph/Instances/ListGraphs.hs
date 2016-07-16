{-
  Convenience forgetful graphs that can be used to build more complex graphs or to examine
  edge or vertex aspect of polymorphic graph.
  Vertices Graph forgets edges,
  Edges graph forgets isolatedVertices.

  Unlike SimpleGraph Edges allow duplicate edges in the list representing muliple edges on a graph   
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

-- Edges intances ------
instance  forall v. (Eq v) => (GraphDataSet (Edges v) v (HPair v) []) where
  isolatedVertices g = []
  edges g  =  getEdges g

instance forall v. (Eq v) => (DiAdjacencyIndex (Edges v) v (HPair v) []) where
  cEdgesOf g ver =  filter(\e -> let HPair(v1,x) = e in v1==ver ) $ getEdges g

instance  forall v. (Eq v) => (DiGraph (Edges v ) v (HPair v) [])

instance  forall v. (Eq v) => (Graph (Edges v ) v (HPair v) [])

--
instance  forall v . (Eq v) => BuildableGraphDataSet (Edges v ) v (HPair v) [] where

   empty = Edges []

   g @+ v = g

   g ~+ e = Edges ( e : (getEdges g))

   union g1 g2 = Edges ((getEdges g1) ++ (getEdges g2))

--------------------------------------------
-- Adjustable Graph instance              --
--------------------------------------------
instance  forall v . (Eq v) => AdjustableGraphDataSet (Edges v ) v (HPair v) [] where

   g @\ f = g

   filterEdges strict g f =
                     let newEdges = filter f (getEdges g)
                     in g {getEdges = newEdges}
