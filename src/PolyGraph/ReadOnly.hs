{-
 Defines the most basic Type Class used by this library to define data that is either Graph or DiGraph.
 A lot of functionality is implemented for GraphDataSets and then reused by both Graph and DiGraph
-}
module PolyGraph.ReadOnly (
   GraphDataSet(..)
   , defaultVertexCount
   , GMorphism (..)
   , fGMorphism
   , isValidGraphDataSet
) where

import Data.List (nub)
import qualified Data.Foldable as F
import qualified Data.Maybe as M

-- |  GraphDataSet thinks of vertices (v) and edges (e) as two arbitrary types
--    Incidence function is defined on Graph or DiGraph level.
--    Foldable constraint does not imply any adjacency friendly folds.
--    It simply means that the content of edges and isolatedVertices can be
--    discovered independently of traversing the graph itself.

class (Eq v, Foldable t)  => GraphDataSet g v e t | g -> t, g -> v, g -> e where
  isolatedVertices ::  g -> t v
  edges            ::  g -> t e

  -- | edge count
  eCount    ::  g -> Int
  eCount g  =  length . edges $ g

  -- | vertex count that uses edge to vertex resolver
  vCount    ::  (e -> (v,v)) -> g -> Int
  vCount    =  defaultVertexCount

-- | property helper
isValidGraphDataSet :: forall g v e t . GraphDataSet g v e t => (e -> (v,v)) ->g -> Bool
isValidGraphDataSet resolveE g =
               let isolatedVs = isolatedVertices g
                   findVMatch :: e -> Bool
                   findVMatch e =
                           let (v1,v2) = resolveE e
                           in  (F.elem v1 isolatedVs) || (F.elem v2 isolatedVs)
               in M.isNothing $ F.find findVMatch (edges g)

----------------------------------------------------------
-- Note GMoriphism can be used with Graphs or DiGraphs  --
-- Definition is has not type contraint on e            --
--                                                      --
-- morphism properties are defined in GraphProperties   --
-- DiGraph.Properties modules separately                --
----------------------------------------------------------
data GMorphism v0 e0 v1 e1 = GMorphism {
   vTrans :: v0 -> v1,
   eTrans :: e0 -> e1
}

fGMorphism :: forall v0 v1 f . (Functor f) => (v0 -> v1) -> GMorphism v0 (f v0) v1 (f v1)
fGMorphism fn = GMorphism {
     vTrans = fn,
     eTrans = fmap fn
 }

-- other helper functions --
defaultVertexCount :: forall g v e t. (GraphDataSet g v e t) => (e -> (v,v)) -> g -> Int
defaultVertexCount f g =
     let isolatedVCount = length . isolatedVertices $ g
         appendVertices :: e -> [v] -> [v]
         appendVertices e list =
                              let (v1, v2) = f e
                              in  v1 : v2 : list
         nonIsolatedVCount = length . nub $ foldr appendVertices [] (edges g)
     in  isolatedVCount + nonIsolatedVCount
