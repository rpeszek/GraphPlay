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
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module GraphPlay.DGraph where --exports everything, a terrible programmer wrote it

import GraphPlay.Helpers
import Data.List (nub)

--
-- e are edges v are vertices, to implememnet direct edge sematics we need to know
-- how to resolve edge into ordered pair of vertices
--
class DEdgeSemantics e v where
  resolveVertices ::  e -> (v,v)  -- semantically resolves vertices edge does not need to be in the graph

--
-- DirectorC one who directs directed graph :) - in child direction
-- It is less than a graph, we can ask for list of child edges at any instance of type v
--
class (DEdgeSemantics e v)  => DirectorC g v e where
  cEdgesOf   ::  g -> v -> [e]   -- return a list of child edges, empty if not a valid vertex or a leaf

--
-- Directed Graph
-- mimics math defintion of being a set of edges and vertices
-- caller can pick which collection type to use as set (Haskell Data.Set is not really a math Set as it requries Ord)
-- Note: Data.Set is not a good representaiton of set since it requires Ord on elements
--
class (Eq v, Foldable t, DEdgeSemantics e v)  => DGraph g v e t where
  vertices ::  g -> t v
  edges    ::  g -> t e

-- let's create a very simple (and slow)  of DirectorC class for testing
-- Note: runSimpleGraph is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'runSimpleGraph sg'
--
newtype SimpleGraph v = SimpleGraph { runSimpleGraph:: [(v,v)]}

--
-- instances
--

-- TODO default implementation of DirectorC under Eq predicate?

instance forall v . (Eq v) => (DEdgeSemantics  (v,v) v) where
  resolveVertices e = e                                                   --(:t) g -> e -> (v,v), brain teaser why is that?

instance forall v . (Eq v) => (DirectorC (SimpleGraph v) v (v,v)) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . runSimpleGraph $ g  --(:t) g -> v -> [e]

instance  forall v . (Eq v) => (DGraph (SimpleGraph v) v (v,v) []) where
  vertices g =  nub . (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . runSimpleGraph $ g
  edges g  =  runSimpleGraph $ g
