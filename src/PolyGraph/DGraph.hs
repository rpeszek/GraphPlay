--
--  This program contains non-memoized implementation of what we call in IPaC a (or the) GraphFold
--  This code is purely functional, no variable state is mutated.
--
--  To make it faster a directed graph traversal would need to remember all visited vertices
--  and return computation result from that vertex next time the vertex is visited
--  (e.g. memoization).  I have not figured out yet if that can
--  be done in a pure code (without any state mutation) maybe with something like a State Monad.
--  I have seen pure memoized implementation of things like Fibonacci
--  numbers but general d-graph topology is more complex that Fib (which is effectively a binary tree).
--
--  To move forward I will try to investigate pure and non-pure faster implementations
--  of this code.
--  I also will want to see if I can make this code even more polymorphic.
--  Notice that the implementation of dfsFold is a bunch of 'map'-s and function compositions (.).
--  These can be generalized (especially map).  So it will be interesting
--  to think if this code can be made even more general and thus stronger typed.
--
--  I added bunch of comments that should make reading this code easier for developers
--  who have not seen Haskell before.
--
--  Note: LANGUAGE extensions shown below are typically configured globally for a project
--

module PolyGraph.DGraph where --exports everything, a terrible programmer wrote it

import PolyGraph.Helpers
import Data.List (nub)

--
-- e are edges v are vertices, to implememnet direct edge sematics we need to know
-- how to resolve edge into ordered pair of vertices
--
class DEdgeSemantics e v | e -> v where
  resolveVertices ::  e -> (v,v)  -- semantically resolves vertices edge does not need to be in the graph

--
-- CIndex one who directs directed graph :) - in child direction
-- It is less than a graph, we can ask for list of child edges at any instance of type v
-- caller picks with Traversable to use for navigatigaging children
--
class (Traversable t, DEdgeSemantics e v)  => CIndex g v e t | g -> t, g -> v, e -> v where
  cEdgesOf   ::  g -> v -> t e   -- return a list of child edges, empty if not a valid vertex or a leaf

--
-- Directed Graph
-- mimics math defintion of being a set of d-edges and vertices
-- caller can pick which collection type to use as set (Haskell Data.Set is not really a math Set as it requries Ord)
-- Note: Data.Set is not a good representaiton of set since it requires Ord on elements
--
class (Eq v, Foldable t, DEdgeSemantics e v)  => DGraph g v e t | g -> t, g -> v, e -> v where
  vertices ::  g -> t v
  edges    ::  g -> t e


--
-- instances
--

-- TODO default implementation of CIndex under Eq predicate?

instance forall v . (Eq v) => (DEdgeSemantics  (v,v) v) where
  resolveVertices e = e                                                   --(:t) g -> e -> (v,v), brain teaser why is that?


-- needs work, not efficient anyway, needs fast indexing of graph
--instance forall v t. (Eq v, Traversable t, Applicative t, Monoid (t (v,v))) => (CIndex (SimpleGraph v t) v (v,v) t) where
--    cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

-- misses nub, it is not efficient anyway
--instance  forall v t . ( Eq v, Foldable t, Monoid (t (v,v))) => (DGraph (SimpleGraph v t) v (v,v) t) where
--  vertices g =  (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) mempty) . getEdges $ g
--  edges g  =  getEdges $ g
