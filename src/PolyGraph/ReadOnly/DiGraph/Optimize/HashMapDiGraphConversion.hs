------
-- TODO optimized index should use PolyGraph.Instances.DiGraph.HashMapAsDiGraph
------
module PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion (
   buildDiGraphHashMap
   --, DiAdjacencyIndexHelper(..)
   --, buildHmDiAdjacencyIndex
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
import PolyGraph.Common.InstanceHelpers

-----------------------------------------------------------------------
-- builders that create fast DiGraph and DiAdjacencyIndex for a DiGraph       ---
-----------------------------------------------------------------------

buildDiGraphHashMap :: forall g v e tg th .
      (Eq v, Hashable v, GraphDataSet g v e tg,
             Traversable th, BuildableDependentCollection (th (EdgeHelper e v)) (EdgeHelper e v)) =>
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



data DiAdjacencyIndexHelper v e t = DiAdjacencyIndexHelper {
    helpercEdgesOf :: v -> t e
}

instance forall v e t . (Hashable v, Eq v, DiEdgeSemantics e v, Traversable t) => DiAdjacencyIndex (DiAdjacencyIndexHelper v e t) v e t where
   cEdgesOf = helpercEdgesOf

class (Eq k) => BuildableMap i k v | i -> k, i-> v where
   safeKeyOverValue ::  i -> k -> (v -> v) -> v ->  i
   emptyMap ::  i
   safeLookup ::  i -> k -> v -> v

-- TODO Depreciated, replace with Common.Helpers.BuildableDependentCollection --
class (Foldable t) => BuildableCollection t  where
   prependElement  :: e -> t e -> t e
   emptyCollection :: t e

buildMap :: forall g v e t0 t i. (DiGraph g v e t0, BuildableCollection t, Traversable t, BuildableMap i v (t e)) => g -> i
buildMap g =
  let gedges = edges g  :: t0 e
      i0  = emptyMap    :: i
  in
      foldr (\e i -> safeKeyOverValue i (first' . resolveDiEdge $ e) (prependElement e) emptyCollection) i0 gedges

--
-- specialize implementation using HashMap
--

buildHM :: forall g v e t0 t i. (Eq v, Hashable v, DiGraph g v e t0, BuildableCollection t, Traversable t) => g -> HM.HashMap v (t e)
buildHM = buildMap

buildHmDiAdjacencyIndex :: forall g v e t0 t. (Hashable v, Eq v, DiGraph g v e t0, BuildableCollection t, Traversable t) => g -> DiAdjacencyIndexHelper v e t
buildHmDiAdjacencyIndex g =
    let hm = buildHM g :: HM.HashMap v (t e)
        cEdgesOfImpl = (\v -> HM.lookupDefault emptyCollection v hm) :: v -> t e
    in DiAdjacencyIndexHelper { helpercEdgesOf = cEdgesOfImpl}

--- Helper instances For DiAdjacencyIndexing work ---

instance forall k v. (Hashable k, Eq k) => BuildableMap (HM.HashMap k v) k v where
  safeKeyOverValue hm k f defaultV =
      let safeVal = HM.lookupDefault defaultV k hm
      in HM.insert k (f safeVal) hm

  emptyMap = HM.empty
  safeLookup hm k v = HM.lookupDefault v k hm

instance BuildableCollection [] where
  prependElement = (:)
  emptyCollection = []

-- HashMap itself is naturally a DiGraph --
-- TODO finish this and add HM to the DiAdjacencyIndexHelper, externalize this to own module
{-
instance forall e v t. (Eq v, Hashable v, BuildableCollection t) => PB.BuildableGraphDataSet(HM.HashMap v (t e)) v e t where
  PB.empty = HM.empty
  --PB.(@+) :: g -> v -> g
  g PB.@+ v  =
              let newVertices =  v `prependElement` (helperVertices g)
              in g {helperVertices = newVertices}

  g PB.(~+) e =
               let newEdges = e `helperEdges` (helperEdges g)
                   newVertices = undefined -- TODO we need something like BuildableUniqueCollection aka HashSet
-}
