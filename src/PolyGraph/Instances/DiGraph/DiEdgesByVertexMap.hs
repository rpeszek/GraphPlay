{-
  HashMap representation of Di-Graph where vertices are keys and collection of di-adjecent edges
  is stored as a value.  Efficient for traversing algorithms.
-}

module PolyGraph.Instances.DiGraph.DiEdgesByVertexMap where

import Data.Hashable
import PolyGraph.Common
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable
import PolyGraph.Adjustable
import PolyGraph.Common.BuildableCollection
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F

newtype DiEdgesByVertexMap v e t = DiEdgesByVertexMap { getHashMap :: HM.HashMap v (t e) } deriving Show



------------------------------------------------------------------------------------------
-- HashMap is naturally a di-graph with [] being the Foldable for edges/vertices        --
------------------------------------------------------------------------------------------
instance forall v e te. (Eq v, Hashable v, Traversable te) =>
                                           (GraphDataSet (DiEdgesByVertexMap v e te) v e []) where
  isolatedVertices =  HM.keys . HM.filter (F.null) . getHashMap
  edges    =  concat . map (F.toList) . HM.elems . getHashMap

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableCollection (te e) e) =>
                                           (DiAdjacencyIndex (DiEdgesByVertexMap v e te) v e te) where
    cEdgesOf g v =  HM.lookupDefault emptyBuildableCollection v (getHashMap g)

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableCollection (te e) e) =>
                                           (DiGraph (DiEdgesByVertexMap v e te) v e [])

instance forall v e te. (Eq v, Hashable v, Traversable te, DiEdgeSemantics e v, BuildableCollection (te e) e) =>
                                  (BuildableGraphDataSet(DiEdgesByVertexMap v e te) v e []) where
   empty = DiEdgesByVertexMap HM.empty
   g @+ v = DiEdgesByVertexMap . (HM.insertWith (\old new -> old) v emptyBuildableCollection) . getHashMap $ g
   g ~+ e =
        let OPair (v1,v2) = resolveDiEdge e
        in DiEdgesByVertexMap .
           (HM.insertWith (\new old -> old) v2 emptyBuildableCollection) .
           (HM.insertWith (\new old -> addBuildableElement e old) v1 (singletonBuildableCollection e)) .
            getHashMap $ g
   union g1 g2 =
              let mergeF :: te e -> te e -> te e
                  mergeF edges1 edges2 = unionBuildableCollections edges1 edges2
              in DiEdgesByVertexMap $ HM.unionWith mergeF (getHashMap g1) (getHashMap g2)

-- this will be a bit slow but not too bad
instance forall v e te. (Eq v, Hashable v, Eq e, Traversable te, DiEdgeSemantics e v, AdjustableCollection (te e) e) =>
                                  (AdjustableGraphDataSet(DiEdgesByVertexMap v e te) v e []) where
   g @\ f =
             let verticesTrimmed = DiEdgesByVertexMap . (HM.filterWithKey (\v _ -> f v)) . getHashMap $ g
                 edgeFilter :: e -> Bool
                 edgeFilter e =
                              let OPair (v1,v2) = resolveDiEdge e
                              in (f v1) && (f v2)
             in DiEdgesByVertexMap . HM.map (filterBuildableCollection edgeFilter) . getHashMap $ verticesTrimmed

   --map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
   filterEdges strict g f =
             let edgesTrimmed = DiEdgesByVertexMap . HM.map (filterBuildableCollection f) . getHashMap $ g
             in if strict
                then DiEdgesByVertexMap . HM.filter (not . F.null) . getHashMap $ edgesTrimmed
                else edgesTrimmed
