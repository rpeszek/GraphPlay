{-
  Convenience forgetful graphs that can be used to build more complex graphs or to examine
  edge or vertex aspect of polymorphic graph.
  Vertices Graph forgets edges,
  Edges graph forgets isolatedVertices. 

  Unlike SimpleGraph Edges allow duplicate edges in the list representing muliple edges on a graph
-}

module Instances.ListGraphs (
  Vertices (..)
  , Edges (..)
  , GEdges
  , DiEdges
) where

import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable
import PolyGraph.Adjustable
import PolyGraph.Common
import Data.List (nub)

-- | this graph has no edges, it acts in forgetful way if edges are added
newtype Vertices v e = Vertices { getVertices :: [v]} deriving Show

-- | this graph has no isolatedVertices, it acts in forgetful way if isolatedVertex is added
newtype Edges v e = Edges { getEdges :: [e] } deriving Show
type  GEdges v  = Edges v (UOPair v)
type  DiEdges v = Edges v (OPair v)



-- instances Vertices --
instance  forall v e. (Eq v)=> (GraphDataSet (Vertices v e) v e []) where
  isolatedVertices g = getVertices g
  edges _  =  []
  -- vCount    ::  (e -> (v,v)) -> g -> Int
  vCount =  const (length . getVertices)

instance forall v e. (DiEdgeSemantics e v) => (DiAdjacencyIndex (Vertices v e) v e []) where
   cEdgesOf _ _ = []

instance forall v e. (EdgeSemantics e v) => (AdjacencyIndex (Vertices v e) v e []) where
   edgesOf _ _ = []

instance  forall v e. (Eq v, DiEdgeSemantics e v) => (DiGraph (Vertices v e) v e [])

instance  forall v e. (Eq v, EdgeSemantics e v) => (Graph (Vertices v e) v e [])


-- instance  forall v e. (Eq v, EdgeSemantics e v) => (Graph (Vertices v e) v e [])

-- adding edge simply ignores edge and adds vertex
instance  forall v e . (Eq v, PairLike e v) => BuildableGraphDataSet (Vertices v e) v e [] where

   empty = Vertices []

   g +@ v = Vertices ( nub (v : (getVertices g)) )
   g +~ e =
            let (v1,v2) = toPair e
            in g +@ v1 +@ v2

   union g1 g2 = Vertices ((getVertices g1) ++ (getVertices g2))

instance  forall v e . (Eq v, Eq e, PairLike e v) => AdjustableGraphDataSet (Vertices v e) v e [] where

   g \@ f = Vertices ( filter f (getVertices g) )

   filterEdges _ g _ = g

-- Edges intances ------
instance  forall v e. (Eq v) => (GraphDataSet (Edges v e) v e []) where
  isolatedVertices _ = []
  edges   =  getEdges 

instance forall v. (Eq v) => (DiAdjacencyIndex (Edges v (OPair v)) v (OPair v) []) where
  cEdgesOf g ver =  filter(\e -> let OPair(v1,_) = e in v1==ver ) $ getEdges g

instance forall v. (Eq v) => (AdjacencyIndex (Edges v (UOPair v)) v (UOPair v) []) where
  edgesOf g ver =  filter(\e -> first e == ver || second e == ver ) $ getEdges g

instance  forall v . (Eq v) => (DiGraph (Edges v (OPair v)) v (OPair v) [])

instance  forall v. (Eq v) => (Graph (Edges v (UOPair v)) v (UOPair v) [])

instance  forall v e. (Eq v) => BuildableGraphDataSet (Edges v e) v e [] where

   empty = Edges []

   g +@ _ = g

   g +~ e = Edges ( e : (getEdges g))

   union g1 g2 = Edges ((getEdges g1) ++ (getEdges g2))

--------------------------------------------
-- Adjustable Graph instance              --
--------------------------------------------
instance  forall v e. (Eq v, Eq e, PairLike e v) => AdjustableGraphDataSet (Edges v e) v e [] where

   g \@ _ = g

   filterEdges _ g f =
                     let newEdges = filter f (getEdges g)
                     in g {getEdges = newEdges}
