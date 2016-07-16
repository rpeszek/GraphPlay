module PolyGraph.Buildable.PolyMorth where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild


morthEdges  :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                GMorphism v0 e0 v1 e1 -> g0 -> g1 -> g1

morthEdges trans gs gd =
                 let edgeF :: e0 -> g1 -> g1
                     edgeF e0 g1 = g1 ~+ (eTrans trans e0)
                 in foldr edgeF gd (edges gs)

morthIsolatedVertices :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                GMorphism v0 e0 v1 e1 -> g0 -> g1 -> g1

morthIsolatedVertices trans gs gd =
                 let vertF :: v0 -> g1 -> g1
                     vertF v0 g1 = g1 @+ (vTrans trans v0)
                 in foldr vertF gd (isolatedVertices gs)

-- | uses graph morphism to create a polymorphic BuildableGraphDataSet
morth :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                GMorphism v0 e0 v1 e1 -> g0 -> g1
morth trans gs = (morthEdges trans gs) . (morthIsolatedVertices trans gs) $ emptyGraph

-- | polyMorth for Functor edges
fmorth :: forall f g0 v0 e0 t0 g1 v1 e1 t1. (Functor f, GraphDataSet g0 v0 (f v0) t0, BuildableGraphDataSet g1 v1 (f v1) t1) =>
                (v0 -> v1) -> g0 -> g1
fmorth f = morth (fGMorphism f)


--
-- NOTE this type of expression will not compile with functionally dependent types because g has a wrong kind
--
-- instance (Functor f, BuildableGraphDataSet g v (f v) t) => Functor g where
--  fmap = fmorth
