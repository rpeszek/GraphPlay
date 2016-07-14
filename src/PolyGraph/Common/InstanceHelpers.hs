
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

   unionDependentCollection :: t -> t -> t

instance forall e. BuildableDependentCollection [e] e where
   prependDependentElement  = (:)
   emptyDependentCollection = []
   unionDependentCollection = (++)

instance forall e hs. (Eq e, Hashable e) => BuildableDependentCollection (HS.HashSet e) e where
   prependDependentElement   = HS.insert
   emptyDependentCollection  = HS.empty
   unionDependentCollection  = HS.union

--Example usage:---
testF :: forall t0 e. (Foldable t0, BuildableDependentCollection (t0 e) e) => t0 e
testF = emptyDependentCollection

class BuildableDependentCollection t e => BuildableUniqueDependentCollection t e  where
   addUniqueElement  :: e -> t  -> t
   uniqueCollectionFromList :: [e] -> t

instance forall e . (Eq e ) => BuildableUniqueDependentCollection [e] e where
   addUniqueElement e list = nub (e: list)
   uniqueCollectionFromList = nub

instance forall e . (Eq e, Hashable e) => BuildableUniqueDependentCollection (HS.HashSet e) e where
   addUniqueElement = HS.insert
   uniqueCollectionFromList = HS.fromList


class (Eq e, BuildableUniqueDependentCollection t e) => AdjustableDependentCollection t e  where
   filterDependentCollection :: (e -> Bool) -> t -> t
   deleteFromDependentCollection :: e -> t -> t
   deleteFromDependentCollection e t = filterDependentCollection (== e) t

instance forall e . (Eq e) => AdjustableDependentCollection [e] e where
   filterDependentCollection = filter

instance forall e . (Eq e, Hashable e) => AdjustableDependentCollection (HS.HashSet e) e where
   filterDependentCollection = HS.filter
   deleteFromDependentCollection = HS.delete


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show
