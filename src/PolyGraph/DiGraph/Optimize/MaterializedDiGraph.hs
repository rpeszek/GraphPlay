
module PolyGraph.DiGraph.Optimize.MaterializedDiGraph (
   DiGraphHelper(..)
   , buidDiGraph
) where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Helpers
import PolyGraph.DiGraph
import PolyGraph.Graph
import PolyGraph.DiGraph.Optimize.HashMapCIndex (BuildableCollection)
import PolyGraph.DiGraph.Optimize.MaterializedDiEdge

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
