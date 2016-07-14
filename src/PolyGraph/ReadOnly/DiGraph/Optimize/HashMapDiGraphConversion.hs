------
-- TODO optimized index should use PolyGraph.Instances.DiGraph.HashMapAsDiGraph
------
module PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion (
   buildDiGraphHashMap
) where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Instances.DiGraph.HashMapAsDiGraph
import PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge
import PolyGraph.Common.Helpers
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.ReadOnly.Graph
import PolyGraph.Common.BuildableCollection

-----------------------------------------------------------------------
-- builders that create fast DiGraph and DiAdjacencyIndex for a DiGraph       ---
-----------------------------------------------------------------------

buildDiGraphHashMap :: forall g v e tg th .
      (Eq v, Hashable v, GraphDataSet g v e tg,
             Traversable th, BuildableCollection (th (EdgeHelper e v)) (EdgeHelper e v)) =>
                      (e -> (v,v)) -> g -> DiGraphHashMap v (EdgeHelper e v) th
buildDiGraphHashMap slowSemantics g =
     let gedges = edges g                         :: tg e
         ginsolatedVertices  = isolatedVertices g :: tg v
         emptyMap = HM.empty                      :: HM.HashMap v (th (EdgeHelper e v))
         emptyEdges = emptyBuildableCollection    :: th (EdgeHelper e v)
         addEdge e hm =
             let (v1,v2) = slowSemantics e
             in (HM.insertWith (\new old -> old) v2 emptyBuildableCollection) .
                (HM.insertWith (\new old -> addBuildableElement (EdgeHelper e (v1,v2)) old) v1 (singletonBuildableCollection (EdgeHelper e (v1,v2)))) $ hm

         verticesInserted = foldr(\v hm -> HM.insert v emptyEdges hm) emptyMap ginsolatedVertices
     in
         DiGraphHashMap $ foldr addEdge verticesInserted gedges
