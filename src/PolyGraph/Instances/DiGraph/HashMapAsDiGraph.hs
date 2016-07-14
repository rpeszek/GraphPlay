-- TODO work in progress, almost works

module PolyGraph.Instances.DiGraph.HashMapAsDiGraph where

import Data.Hashable
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.InstanceHelpers
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F

newtype DiGraphHashMap v e t = DiGraphHashMap { getHashMap :: HM.HashMap v (t e) } deriving Show



------------------------------------------------------------------------------------------
-- HashMap is naturally a di-graph with [] being the Foldable for edges/vertices        --
------------------------------------------------------------------------------------------
instance forall v e te. (Eq v, Hashable v, Traversable te) =>
                                           (GraphDataSet (DiGraphHashMap v e te) v e []) where
  isolatedVertices =  HM.keys . HM.filter (F.null) . getHashMap
  edges    =  concat . map (F.toList) . HM.elems . getHashMap

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                           (DiAdjacencyIndex (DiGraphHashMap v e te) v e te) where
    cEdgesOf g v =  HM.lookupDefault emptyBuildableCollection v (getHashMap g)

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                           (DiGraph (DiGraphHashMap v e te) v e [])

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                  (BuildableGraphDataSet(DiGraphHashMap v e te) v e []) where
   empty = DiGraphHashMap HM.empty
   g @+ v = DiGraphHashMap . (HM.insertWith (\old new -> old) v emptyBuildableCollection) . getHashMap $ g
   g ~+ e =
        let (v1,v2) = resolveDiEdge e
        in DiGraphHashMap .
           (HM.insertWith (\new old -> old) v2 emptyBuildableCollection) .
           (HM.insertWith (\new old -> addBuildableElement e old) v1 (singletonBuildableCollection e)) .
            getHashMap $ g
   union g1 g2 =
              let mergeF :: te e -> te e -> te e
                  mergeF edges1 edges2 = unionBuildableCollections edges1 edges2
              in DiGraphHashMap $ HM.unionWith mergeF (getHashMap g1) (getHashMap g2)

-- this will be a bit slow but not too bad
instance forall v e te. (Eq v, Hashable v, Eq e, Traversable te, DiEdgeSemantics e v, AdjustableDependentCollection (te e) e) =>
                                  (AdjustableGraphDataSet(DiGraphHashMap v e te) v e []) where
   g @\ f =
             let verticesTrimmed = DiGraphHashMap . (HM.filterWithKey (\v _ -> f v)) . getHashMap $ g
                 edgeFilter :: e -> Bool
                 edgeFilter e =
                              let (v1,v2) = resolveDiEdge e
                              in (f v1) && (f v2)
             in DiGraphHashMap . HM.map (filterBuildableCollection edgeFilter) . getHashMap $ verticesTrimmed
   --map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
   filterEdges strict g f =
             let edgesTrimmed = DiGraphHashMap . HM.map (filterBuildableCollection f) . getHashMap $ g
             in if strict
                then DiGraphHashMap . HM.filter (not . F.null) . getHashMap $ edgesTrimmed
                else edgesTrimmed
