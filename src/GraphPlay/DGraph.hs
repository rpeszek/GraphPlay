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
--
class (DEdgeSemantics e v, Eq v)  =>  DGraph g v e where
  validVetex ::  g -> v -> Bool
  edgesOf    ::  g -> v -> [e]   -- returns a of all edges (children and parents), empty if not a valid vertex or disconnected vertex

-- let's create a very simple (and slow)  of DirectorC class for testing
-- Note: runSimpleGraph is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'runSimpleGraph sg'
--
newtype SimpleGraph v = SimpleGraph { runSimpleGraph:: [(v,v)]}

--
-- instances
--
instance forall g v e . (DGraph g v e) => DirectorC g v e where
   cEdgesOf g v = filter (\e -> v == ((first' . resolveVertices) e)) (edgesOf g v)

instance forall v . (Eq v) => (DEdgeSemantics  (v,v) v) where
  resolveVertices e = e                                                   --(:t) g -> e -> (v,v), brain teaser why is that?

-- why do I need that DGraph should be enough?  do I need another pragma?
instance forall v . (Eq v) => (DirectorC (SimpleGraph v) v (v,v)) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . runSimpleGraph $ g  --(:t) g -> v -> [e]

instance forall v . (Eq v) => (DGraph (SimpleGraph v) v (v,v)) where
  validVetex g ver =  foldr (\vv b -> b || (first' vv == ver) || (second' vv == ver)) False $ runSimpleGraph g  --foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
  edgesOf g ver = filter (\vv -> (first' vv == ver) || (second' vv == ver)) . runSimpleGraph $ g  --(:t) g -> v -> [e]
