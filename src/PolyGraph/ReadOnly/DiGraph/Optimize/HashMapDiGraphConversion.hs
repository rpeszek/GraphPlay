------
-- TODO optimized index should use PolyGraph.Instances.DiGraph.DiEdgesByVertexMap
------
module PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion (
   buildDiEdgesByVertexMap
) where

import qualified Data.Maybe as MB
--import Control.Monad
--import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict                        as HM
import PolyGraph.Instances.DiGraph.DiEdgesByVertexMap       (DiEdgesByVertexMap(..))
import PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge (EdgeHelper(..))
import PolyGraph.Common                                     (OPair(..))
import qualified PolyGraph.ReadOnly                         as Base
import qualified PolyGraph.Common.BuildableCollection       as Bcol

-----------------------------------------------------------------------
-- builders that create fast DiGraph and DiAdjacencyIndex for a DiGraph       ---
-----------------------------------------------------------------------

buildDiEdgesByVertexMap :: forall g v e tg th .
      (Eq v, Hashable v, Base.GraphDataSet g v e tg,
             Traversable th, Bcol.BuildableCollection (th (EdgeHelper e v)) (EdgeHelper e v)) =>
                      (e -> (OPair v)) -> g -> DiEdgesByVertexMap v (EdgeHelper e v) th
buildDiEdgesByVertexMap slowSemantics g =
     let gedges = Base.edges g                         :: tg e
         ginsolatedVertices  = Base.isolatedVertices g :: tg v
         emptyMap = HM.empty                           :: HM.HashMap v (th (EdgeHelper e v))
         emptyEdges = Bcol.emptyBuildableCollection    :: th (EdgeHelper e v)
         addEdge e hm =
             let OPair (v1,v2) = slowSemantics e
             in (HM.insertWith (\new old -> old) v2 Bcol.emptyBuildableCollection) .
                (HM.insertWith (\new old -> Bcol.addBuildableElement (EdgeHelper e (OPair (v1,v2))) old) v1 (Bcol.singletonBuildableCollection (EdgeHelper e (OPair (v1,v2))))) $ hm

         verticesInserted = foldr(\v hm -> HM.insert v emptyEdges hm) emptyMap ginsolatedVertices
     in
         DiEdgesByVertexMap $ foldr addEdge verticesInserted gedges
