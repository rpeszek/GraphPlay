
module PolyGraph.DGraph.Indexers (
   BuildableCollection
   , buildHmCIndex
   , CIndexContainer
   , emptyFastDEgdes
   , buidFastDEdges
   , FastDEdge(..)
   , fastVertices
) where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Helpers
import PolyGraph.DGraph

-----------------------------------------------------------------------
-- builders that create fast CIndex implemenations for any DGraph   ---
-----------------------------------------------------------------------

data CIndexContainer v e t = CIndexContainer {
    indexedcEdgesOf :: v -> t e
}

instance forall v e t . (Hashable v, Eq v, DEdgeSemantics e v, Traversable t) => CIndex (CIndexContainer v e t) v e t where
   cEdgesOf = indexedcEdgesOf

class (Eq k) => BuildableMap i k v | i -> k, i-> v where
   safeKeyOverValue ::  i -> k -> (v -> v) -> v ->  i
   emptyMap ::  i
   safeLookup ::  i -> k -> v -> v

class (Foldable t) => BuildableCollection t  where
   prependElement  :: e -> t e -> t e
   emptyCollection :: t e

buildMap :: forall g v e t0 t i. (DGraph g v e t0, BuildableCollection t, Traversable t, BuildableMap i v (t e)) => g -> i
buildMap g =
  let gedges = edges g  :: t0 e
      i0  = emptyMap    :: i
  in
      foldr (\e i -> safeKeyOverValue i (first' . resolveVertices $ e) (prependElement e) emptyCollection) i0 gedges

--
-- specialize implementation using HashMap
--

buildHM :: forall g v e t0 t i. (Eq v, Hashable v, DGraph g v e t0, BuildableCollection t, Traversable t) => g -> HM.HashMap v (t e)
buildHM = buildMap

buildHmCIndex :: forall g v e t0 t. (Hashable v, Eq v, DGraph g v e t0, BuildableCollection t, Traversable t) => g -> CIndexContainer v e t
buildHmCIndex g =
    let hm = buildHM g :: HM.HashMap v (t e)
        cEdgesOfImpl = (\v -> HM.lookupDefault emptyCollection v hm) :: v -> t e
    in CIndexContainer { indexedcEdgesOf = cEdgesOfImpl}

--- Helper instances For CIndexing work ---

instance forall k v. (Hashable k, Eq k) => BuildableMap (HM.HashMap k v) k v where
  safeKeyOverValue hm k f defaultV =
      let safeVal = HM.lookupDefault defaultV k hm
      in HM.insert k (f safeVal) hm

  emptyMap = HM.empty
  safeLookup hm k v = HM.lookupDefault v k hm

instance  BuildableCollection [] where
  prependElement = (:)
  emptyCollection = []


-----------------------------------------------------------------------------------------
--  DEdgeSemantics indexer should be needed only if rerieval of v-s from edges is slow --
-----------------------------------------------------------------------------------------

data FastDEdge e v = DEdgeWithIndexedSemantics {
    getDEdge  :: e,
    getVertices   :: (v,v)
}

instance forall e v.(Eq v) => Eq(FastDEdge e v) where
  fedge1 == fedge2 = (getVertices fedge1) == (getVertices fedge2)


instance forall v e map. DEdgeSemantics (FastDEdge e v) v where
   resolveVertices indexedE = getVertices indexedE

emptyFastDEgdes :: forall t e v . (BuildableCollection t) =>  t (FastDEdge e v)
emptyFastDEgdes = emptyCollection

buidFastDEdges :: forall t e v t0. (Foldable t, BuildableCollection t0) =>
                             t0 (FastDEdge e v) ->  (e -> (v,v)) -> t e -> t0 (FastDEdge e v)
buidFastDEdges _emptyCollection _slowResolveVertices _slowEdgeCollection =
        --foldr :: (a -> b -> b) -> b -> t a -> b
        foldr (\oldEdge inxEdges ->
                  let newEdge = DEdgeWithIndexedSemantics {getDEdge = oldEdge, getVertices = _slowResolveVertices oldEdge}
                  in prependElement newEdge inxEdges) _emptyCollection _slowEdgeCollection



fastVertices' :: forall t t0 v e. (Foldable t, BuildableCollection t0) => t0 v -> t (FastDEdge e v) -> t0 v
fastVertices' empty edges = foldr(\edge vertices -> (prependElement $ (first' . getVertices) edge) . (prependElement $ (second' . getVertices) edge) $ vertices) empty edges

fastVertices  :: forall t t0 v e. (Foldable t, BuildableCollection t0)  => t (FastDEdge e v) -> t0 v
fastVertices = fastVertices' emptyCollection

--collectVertices' :: forall t t0 v . (Foldable t, BuildableCollection t0) => t0 v -> t (v,v) -> t0 v
--collectVertices' empty edges = foldr(\edge vertices -> (prependElement $ first' edge) .(prependElement $ second' edge) $ vertices) empty edges

--collectVertices  :: forall t t0 v . (Foldable t, BuildableCollection t0)  => t (v,v) -> t0 v
--collectVertices = collectVertices' emptyCollection

----------------------------
-- IDEAS that did not work:
----------------------------

--currently not used
--class Benign m where
--  runBenign :: (forall s . m s a) -> a

--instance  Benign ST where
--    runBenign = runST

--removing ST s from paritally applied HashTable s v [e] is no allowed by typechecker. s-s do not much

-- this will not work because s is not functional dependecy bound
--class (Eq k, Benign m, Monad (m s)) => BuildableMap i k v m s | i -> k, i-> v, i-> m where
