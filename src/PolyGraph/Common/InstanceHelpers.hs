
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
   addBuildableElement  :: e -> t -> t
   emptyBuildableCollection :: t

   singletonBuildableCollection :: e -> t
   singletonBuildableCollection e = addBuildableElement e emptyBuildableCollection

   unionBuildableCollections :: t -> t -> t

instance forall e. BuildableDependentCollection [e] e where
   addBuildableElement  = (:)
   emptyBuildableCollection = []
   unionBuildableCollections = (++)

instance forall e hs. (Eq e, Hashable e) => BuildableDependentCollection (HS.HashSet e) e where
   addBuildableElement   = HS.insert
   emptyBuildableCollection  = HS.empty
   unionBuildableCollections  = HS.union

--Example usage:---
testF :: forall t0 e. (Foldable t0, BuildableDependentCollection (t0 e) e) => t0 e
testF = emptyBuildableCollection

class BuildableDependentCollection t e => BuildableUniqueDependentCollection t e  where
   addUniqueBuildableElement  :: e -> t  -> t
   uniqueBuildableCollectionFromList :: [e] -> t

instance forall e . (Eq e ) => BuildableUniqueDependentCollection [e] e where
   addUniqueBuildableElement e list = nub (e: list)
   uniqueBuildableCollectionFromList = nub

instance forall e . (Eq e, Hashable e) => BuildableUniqueDependentCollection (HS.HashSet e) e where
   addUniqueBuildableElement = HS.insert
   uniqueBuildableCollectionFromList = HS.fromList


class (Eq e, BuildableUniqueDependentCollection t e) => AdjustableDependentCollection t e  where
   filterBuildableCollection :: (e -> Bool) -> t -> t
   deleteBuildableElement :: e -> t -> t
   deleteBuildableElement e t = filterBuildableCollection (== e) t

instance forall e . (Eq e) => AdjustableDependentCollection [e] e where
   filterBuildableCollection = filter

instance forall e . (Eq e, Hashable e) => AdjustableDependentCollection (HS.HashSet e) e where
   filterBuildableCollection = HS.filter
   deleteBuildableElement = HS.delete


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show
