
module PolyGraph.DGraph.Indexers where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import PolyGraph.Helpers
import PolyGraph.DGraph

data CIndexContainer v e t = CIndexContainer {
    indexedcEdgesOf :: v -> t e
}

class (Eq k) => BuildableMap i k v | i -> k, i-> v where
   safeKeyOverValue ::  i -> k -> (v -> v) -> v ->  i
   emptyMap ::  i
   safeLookup ::  i -> k -> v -> v

class (Traversable t) => BuildableTraversable t  where
   addElement :: e -> t e -> t e
   emptyCollection :: t e

buildMap :: forall g v e t0 t i. (DGraph g v e t0, BuildableTraversable t, BuildableMap i v (t e)) => g -> i
buildMap g =
  let gedges = edges g  :: t0 e
      i0  = emptyMap    :: i
  in
      foldr (\e i -> safeKeyOverValue i (first' . resolveVertices $ e) (addElement e) emptyCollection) i0 gedges

--
-- specialize implementation using HashMap
--

buildHM :: forall g v e t0 t i. (Eq v, Hashable v, DGraph g v e t0, BuildableTraversable t) => g -> HM.HashMap v (t e)
buildHM = buildMap

buildHmIndex :: forall g v e t0 t. (Hashable v, Eq v, DGraph g v e t0, BuildableTraversable t) => g -> CIndexContainer v e t
buildHmIndex g =
    let hm = buildHM g :: HM.HashMap v (t e)
        cEdgesOfImpl = (\v -> HM.lookupDefault emptyCollection v hm) :: v -> t e
    in CIndexContainer { indexedcEdgesOf = cEdgesOfImpl}

--- INSTANCES ---

instance forall k v. (Hashable k, Eq k) => BuildableMap (HM.HashMap k v) k v where
  safeKeyOverValue hm k f defaultV =
      let safeVal = HM.lookupDefault defaultV k hm
      in HM.insert k (f safeVal) hm

  emptyMap = HM.empty
  safeLookup hm k v = HM.lookupDefault v k hm

instance  BuildableTraversable [] where
  addElement = (:)
  emptyCollection = []

instance forall v e t . (Hashable v, Eq v, DEdgeSemantics e v, Traversable t) => CIndex (CIndexContainer v e t) v e t where
   cEdgesOf = indexedcEdgesOf

----------------------------
-- IDEAS that did not work:
----------------------------

--currently not used
class Benign m where
  runBenign :: (forall s . m s a) -> a

instance  Benign ST where
    runBenign = runST

--removing ST s from paritally applied HashTable s v [e] is no allowed by typechecker. s-s do not much

-- this will not work because s is not functional dependecy bound
--class (Eq k, Benign m, Monad (m s)) => BuildableMap i k v m s | i -> k, i-> v, i-> m where
