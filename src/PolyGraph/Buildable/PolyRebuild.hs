module PolyGraph.Buildable.PolyRebuild where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild

data GraphTrans e0 v0 e1 v1 = GraphTrans {
     eResolver  :: e0 -> (v0, v0),
     vTransform :: v0 -> v1,
     eBuilder   :: v1 -> v1 -> e1
}

transformAllEdges :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                GraphTrans e0 v0 e1 v1 -> g0 -> g1 -> g1
transformAllEdges trans gs gd =
                 let edgeF :: e0 -> g1 -> g1
                     edgeF e0 g1 =
                                  let (v0_1, v0_2) = (eResolver trans) e0
                                      v1_1 = (vTransform trans) v0_1
                                      v1_2 = (vTransform trans) v0_2
                                      e1 = (eBuilder trans) v1_1 v1_2
                                  in g1 ~+ e1
                 in foldr edgeF gd (edges gs)

transformAllIsolatedVertices :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                  (v0 -> v1) -> g0 -> g1 -> g1
transformAllIsolatedVertices vTrans gs gd =
                    let vertF :: v0 -> g1 -> g1
                        vertF v0 g1 = g1 @+ (vTrans v0)
                    in foldr vertF gd (isolatedVertices gs)

polyTransform :: forall g0 v0 e0 t0 g1 v1 e1 t1. (GraphDataSet g0 v0 e0 t0, BuildableGraphDataSet g1 v1 e1 t1) =>
                GraphTrans e0 v0 e1 v1 -> g0 -> g1
polyTransform trans gs = (transformAllEdges trans gs) . (transformAllIsolatedVertices (vTransform trans) gs) $ emptyGraph

addAllEdges :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                   g0 -> g1 -> g1
addAllEdges gs gd =  foldr(flip (~+)) gd (edges gs)

addAllIsolatedVertices :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                  g0 -> g1 -> g1
addAllIsolatedVertices = transformAllIsolatedVertices id

polyRebuild :: forall g0 v e t0 g1 t1. (GraphDataSet g0 v e t0, BuildableGraphDataSet g1 v e t1) =>
                  g0 -> g1
polyRebuild gs =  (addAllEdges gs) . (addAllIsolatedVertices gs) $ emptyGraph
