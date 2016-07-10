
module PolyGraph.DiGraph.Optimize.HashMapCIndex (
   BuildableCollection (..)
   , CIndexHelper(..)
   , buildHmCIndex
) where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Helpers
import PolyGraph.DiGraph
import PolyGraph.Graph

-----------------------------------------------------------------------
-- builders that create fast CIndex implemenations for any DiGraph  ---
-- somewhat flexible implementation allowing to swap HashMap        ---
-- implementation of HashMap                                        ---
-----------------------------------------------------------------------

data CIndexHelper v e t = CIndexHelper {
    helpercEdgesOf :: v -> t e
}

instance forall v e t . (Hashable v, Eq v, DiEdgeSemantics e v, Traversable t) => CIndex (CIndexHelper v e t) v e t where
   cEdgesOf = helpercEdgesOf

class (Eq k) => BuildableMap i k v | i -> k, i-> v where
   safeKeyOverValue ::  i -> k -> (v -> v) -> v ->  i
   emptyMap ::  i
   safeLookup ::  i -> k -> v -> v

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

buildHmCIndex :: forall g v e t0 t. (Hashable v, Eq v, DiGraph g v e t0, BuildableCollection t, Traversable t) => g -> CIndexHelper v e t
buildHmCIndex g =
    let hm = buildHM g :: HM.HashMap v (t e)
        cEdgesOfImpl = (\v -> HM.lookupDefault emptyCollection v hm) :: v -> t e
    in CIndexHelper { helpercEdgesOf = cEdgesOfImpl}

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
