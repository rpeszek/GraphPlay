
module GraphPlay.DGraph.Indexers where

import qualified Data.Maybe as MB
import Control.Monad
import Control.Monad.ST
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import GraphPlay.Helpers
import GraphPlay.DGraph

data CIndexContainer v e t = CIndexContainer {
    indexedcEdgesOf :: v -> t e
}

class (Eq k) => BuildableMap i k v | i -> k, i-> v where
   safeKeyOverValue ::  i -> k -> (v -> v) -> v ->  i
   emptyMap ::  i
   lookupDefault ::  i -> k -> v -> v

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

indexWithHM :: forall g v e t0 t. (Hashable v, Eq v, DGraph g v e t0, BuildableTraversable t) => g -> CIndexContainer v e t
indexWithHM g =
    let hm = buildHM g :: HM.HashMap v (t e)
        cEdgesOfImpl = (\v -> HM.lookupDefault emptyCollection v hm) :: v -> t e
    in CIndexContainer { indexedcEdgesOf = cEdgesOfImpl}

---buildFcEdgesF :: forall g v e t0 t. (Hashable v, Eq v, DGraph g v e t0, BuildableTraversable t) => g ->  v -> t e
--buildFcEdgesF g v =
--      let hm = (buildHM g) :: BuildableMap i v (t e)
--      in lookupDefault hm v emptyMap
{-
-- this polymorphism does not work, it appears that ST s type trick prevents typechecker from accepting it
-- createIndex:: forall s g v e f t m i. (Benign m, DGraph g v e f,  BuildableTraversable t e, BuildableMap i v (t e) (m s)) => g -> i
-- createIndex g =  runBenign . createIndexM $ g

--
-- Implementation specializing to HashTable and []
--
type HashTable s k v = C.HashTable s k v

--CIndex g v e t
createHtOfListsIndexST:: forall g v e t s. (Hashable v, Eq v, DGraph g v e t) => g -> ST s (HashTable s v [e])
createHtOfListsIndexST = createIndexM

cEdgesOfUsingHTCIndexST :: forall s v e. (Eq v, Hashable v) => HashTable s v [e] -> v -> ST s [e]
cEdgesOfUsingHTCIndexST ht v =
      let stResult = (do
              maybe_te <- H.lookup ht v
              let te = (MB.fromMaybe emptyCollection maybe_te) :: [e]
              return te) :: ST s [e]
      in stResult


cEdgesOfUsingHTCIndex :: forall s v e. (Eq v, Hashable v) => HashTable s v [e] -> v -> [e]
cEdgesOfUsingHTCIndex ht v = runST $ cEdgesOfUsingHTCIndexST ht v

createHtBasedIndexerF :: forall g v e t s. (Hashable v, Eq v, DGraph g v e t) => g -> ST s (v -> [e])
createHtBasedIndexerF g =  do
   ht <- createHtOfListsIndexST g
   return (cEdgesOfUsingHTCIndex ht)

data HTCIndex v e = HTCIndex {
    -- graphdata :: ST s (HashTable s v [e])
    cEdgesOfUsingHt :: v -> [e]
}

-}

--- INSTANCES ---

instance forall k v. (Hashable k, Eq k) => BuildableMap (HM.HashMap k v) k v where
  safeKeyOverValue hm k f defaultV =
      let safeVal = HM.lookupDefault defaultV k hm
      in HM.insert k (f safeVal) hm

  emptyMap = HM.empty
  lookupDefault hm k v = HM.lookupDefault v k hm

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
