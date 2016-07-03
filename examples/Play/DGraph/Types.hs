module Play.DGraph.Types (
   SimpleGraph(..)
) where

import PolyGraph.DGraph
import PolyGraph.Helpers
import Data.List (nub)

-- let's create a very simple (and slow)  of CIndex class for testing
-- Note: getEdges is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'getEdges sg'
--
newtype SimpleGraph v t = SimpleGraph { getEdges:: t (v,v)}

instance  forall v . (Eq v) => (DGraph (SimpleGraph v []) v (v,v) []) where
  vertices g =  nub . (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v []) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]
