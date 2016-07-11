module PolyGraph.Instances.HashMapAsDiGraph where

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
  vertices =  HM.keys . getHashMap
  edges    =  concat . map (F.toList) . HM.elems . getHashMap

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                           (CIndex (DiGraphHashMap v e te) v e te) where
    cEdgesOf g v =  HM.lookupDefault emptyDependentCollection v (getHashMap g)

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                           (DiGraph (DiGraphHashMap v e te) v e [])

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableDependentCollection (te e) e) =>
                                  (BuildableGraphDataSet(DiGraphHashMap v e te) v e []) where
   empty = DiGraphHashMap HM.empty
   g @+ v = DiGraphHashMap . (HM.insertWith (\old new -> old) v emptyDependentCollection) . getHashMap $ g
   g ~+ e =
        let (v1,v2) = resolveDiEdge e
        in DiGraphHashMap .
           (HM.insertWith (\old new -> old) v2 emptyDependentCollection) .
           (HM.insertWith (\old new -> prependDependentElement e old) v1 (singletonDependentCollection e)) .
            getHashMap $ g

-- this will be a bit slow but not too bad
instance forall v e te. (Eq v, Hashable v, Eq e, Traversable te, DiEdgeSemantics e v, AdjustableDependentCollection (te e) e) =>
                                  (AdjustableGraphDataSet(DiGraphHashMap v e te) v e []) where
   g @\ f =
             let verticesTrimmed = DiGraphHashMap . (HM.filterWithKey (\v _ -> f v)) . getHashMap $ g
                 edgeFilter :: e -> Bool
                 edgeFilter e =
                              let (v1,v2) = resolveDiEdge e
                              in (f v1) && (f v2)
             in DiGraphHashMap . HM.map (filterDependentCollection edgeFilter) . getHashMap $ verticesTrimmed
   --map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
   g ~\ f =
             let edgesTrimmed = DiGraphHashMap . HM.map (filterDependentCollection f) . getHashMap $ g
             in  DiGraphHashMap . HM.filter (not . F.null) . getHashMap $ edgesTrimmed
