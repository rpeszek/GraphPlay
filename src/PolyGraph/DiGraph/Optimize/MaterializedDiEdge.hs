
module PolyGraph.DiGraph.Optimize.MaterializedDiEdge (
   DiEdgeHelper(..)
   , buidDiEdgeHelpers
   , appendDiEdgeHelpers
   , emptyDiEdgeHelperCollection
   , collectDiEdgeHelperVertices
) where

import qualified Data.Maybe as MB
import qualified Data.HashMap.Strict as HM
import PolyGraph.Helpers
import PolyGraph.DiGraph
import PolyGraph.Graph
import PolyGraph.DiGraph.Optimize.HashMapCIndex (BuildableCollection(..))

------------------------------------------------------------------------------------------
--  DiEdgeSemantics indexer should be needed only if rerieval of v-s from edges is slow --
-- for example is e -> (v,v) needs to parse a text to lookup vertices                   --
------------------------------------------------------------------------------------------

data DiEdgeHelper e v = DEdgeWithIndexedSemantics {
    getDEdge      :: e,
    getVertices   :: (v,v)
}

instance forall e v.(Eq v) => Eq(DiEdgeHelper e v) where
  fedge1 == fedge2 = (getVertices fedge1) == (getVertices fedge2)


instance forall v e map. DiEdgeSemantics (DiEdgeHelper e v) v where
   resolveDiEdge indexedE = getVertices indexedE

emptyDiEdgeHelperCollection :: forall t e v . (BuildableCollection t) =>  t (DiEdgeHelper e v)
emptyDiEdgeHelperCollection = emptyCollection

appendDiEdgeHelpers :: forall t e v t0. (Foldable t, BuildableCollection t0) =>
                             t0 (DiEdgeHelper e v) ->  (e -> (v,v)) -> t e -> t0 (DiEdgeHelper e v)
appendDiEdgeHelpers _startingCollection _slowresolveDiEdge _slowEdgeCollection =
        --foldr :: (a -> b -> b) -> b -> t a -> b
        foldr (\oldEdge inxEdges ->
                  let newEdge = DEdgeWithIndexedSemantics {getDEdge = oldEdge, getVertices = _slowresolveDiEdge oldEdge}
                  in prependElement newEdge inxEdges) _startingCollection _slowEdgeCollection

buidDiEdgeHelpers :: forall t e v t0. (Foldable t, BuildableCollection t0) => (e -> (v,v)) -> t e -> t0 (DiEdgeHelper e v)
buidDiEdgeHelpers  = appendDiEdgeHelpers emptyCollection


collectDiEdgeHelperVertices' :: forall t t0 v e. (Foldable t, BuildableCollection t0) => t0 v -> t (DiEdgeHelper e v) -> t0 v
collectDiEdgeHelperVertices' empty edges = foldr(\edge vertices -> (prependElement $ (first' . getVertices) edge) . (prependElement $ (second' . getVertices) edge) $ vertices) empty edges

collectDiEdgeHelperVertices  :: forall t t0 v e. (Foldable t, BuildableCollection t0)  => t (DiEdgeHelper e v) -> t0 v
collectDiEdgeHelperVertices = collectDiEdgeHelperVertices' emptyCollection

------------------------------------------------------------------------------------
-- helpers for building a graph from thigs that have slow edge resolution        ---
-- this basically allows for creating a d-graph if you have a slow               ---
-- e-> (v,v) function and a slow Foldable with a list of all edges               ---
-- created graph will not contain any loose vertices                             ---
------------------------------------------------------------------------------------
data DiGraphHelper v e t = DiGraphHelper {
   helperEdges    :: t (DiEdgeHelper e v),
   helperVertices :: t v
}

instance forall e v t. (Eq v, Foldable t) => GraphDataSet(DiGraphHelper v e t) v (DiEdgeHelper e v) t where
  edges    = helperEdges
  vertices = helperVertices

instance forall e v t. (Eq v, Foldable t) => DiGraph(DiGraphHelper v e t) v (DiEdgeHelper e v) t

buidDiGraph :: forall t e v t0. (Foldable t, BuildableCollection t0) =>
                                    (e -> (v,v)) -> t e -> DiGraphHelper v e t0
buidDiGraph _slowresolveDiEdge _slowEdgeCollection =
             let _fastDedges = appendDiEdgeHelpers emptyDiEdgeHelperCollection _slowresolveDiEdge _slowEdgeCollection :: t0 (DiEdgeHelper e v)
                 _collectDiEdgeHelperVertices = collectDiEdgeHelperVertices _fastDedges   ::t0 v
             in DiGraphHelper {helperEdges = _fastDedges, helperVertices = _collectDiEdgeHelperVertices}
