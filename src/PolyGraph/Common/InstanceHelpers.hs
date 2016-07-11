
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Common.InstanceHelpers (
  BuildableDependentCollection (..),
  BuildableUniqueDependentCollection (..),
  AdjustableDependentCollection (..),
  ToString (..)
) where

import qualified Data.HashSet as HS
import Data.Hashable
import Data.List (nub, filter)

-- helper classes  ---
class BuildableDependentCollection t e | t -> e where
   prependDependentElement  :: e -> t -> t
   emptyDependentCollection :: t
   singletonDependentCollection :: e -> t
   singletonDependentCollection e = prependDependentElement e emptyDependentCollection

instance forall e. BuildableDependentCollection [e] e where
   prependDependentElement = (:)
   emptyDependentCollection = []

instance forall e hs. (Eq e, Hashable e) => BuildableDependentCollection (HS.HashSet e) e where
   prependDependentElement   = HS.insert
   emptyDependentCollection  = HS.empty

--Example usage:---
testF :: forall t0 e. (Foldable t0, BuildableDependentCollection (t0 e) e) => t0 e
testF = emptyDependentCollection

class BuildableUniqueDependentCollection t e | t -> e  where
   addUniqueElement  :: e -> t  -> t
   emptyUniqueCollection :: t
   uniqueCollectionFromList :: [e] -> t

instance forall e . (Eq e ) => BuildableUniqueDependentCollection [e] e where
   addUniqueElement e list = nub (e: list)
   emptyUniqueCollection = []
   uniqueCollectionFromList = nub

instance forall e . (Eq e, Hashable e) => BuildableUniqueDependentCollection (HS.HashSet e) e where
   addUniqueElement = HS.insert
   emptyUniqueCollection = HS.empty
   uniqueCollectionFromList = HS.fromList


class BuildableDependentCollection t e => AdjustableDependentCollection t e  where
   filterDependentCollection :: (e -> Bool) -> t -> t

instance forall e . AdjustableDependentCollection [e] e where
   filterDependentCollection = filter

instance forall e . (Eq e, Hashable e) => AdjustableDependentCollection (HS.HashSet e) e where
   filterDependentCollection = HS.filter


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show
