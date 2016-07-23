------
-- TODO optimized index should use PolyGraph.Instances.DiGraph.DiEdgesByVertexMap
------
module PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion (
   buildDiEdgesByVertexMap
) where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Instances.DiGraph.DiEdgesByVertexMap
import PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge
import PolyGraph.Common
import PolyGraph.ReadOnly
import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Common.BuildableCollection

-----------------------------------------------------------------------
-- builders that create fast DiGraph and DiAdjacencyIndex for a DiGraph       ---
-----------------------------------------------------------------------

buildDiEdgesByVertexMap :: forall g v e tg th .
      (Eq v, Hashable v, GraphDataSet g v e tg,
             Traversable th, BuildableCollection (th (EdgeHelper e v)) (EdgeHelper e v)) =>
                      (e -> (OPair v)) -> g -> DiEdgesByVertexMap v (EdgeHelper e v) th
buildDiEdgesByVertexMap slowSemantics g =
     let gedges = edges g                         :: tg e
         ginsolatedVertices  = isolatedVertices g :: tg v
         emptyMap = HM.empty                      :: HM.HashMap v (th (EdgeHelper e v))
         emptyEdges = emptyBuildableCollection    :: th (EdgeHelper e v)
         addEdge e hm =
             let OPair (v1,v2) = slowSemantics e
             in (HM.insertWith (\new old -> old) v2 emptyBuildableCollection) .
                (HM.insertWith (\new old -> addBuildableElement (EdgeHelper e (OPair (v1,v2))) old) v1 (singletonBuildableCollection (EdgeHelper e (OPair (v1,v2))))) $ hm

         verticesInserted = foldr(\v hm -> HM.insert v emptyEdges hm) emptyMap ginsolatedVertices
     in
         DiEdgesByVertexMap $ foldr addEdge verticesInserted gedges
