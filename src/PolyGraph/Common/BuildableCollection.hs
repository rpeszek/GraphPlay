
--
-- SHARED HELPERS
-- various helpers (typically exist defined somewhere else but wanted to limit library dependencies)
--

module PolyGraph.Common.BuildableCollection (
  BuildableCollection (..),
  BuildableUniqueCollection (..),
  AdjustableCollection (..),
  ToString (..)
) where

import qualified Data.HashSet as HS
import Data.Hashable
import Data.List (nub, filter)

-- helper classes  ---
class BuildableCollection t e | t -> e where
   addBuildableElement  :: e -> t -> t
   emptyBuildableCollection :: t

   singletonBuildableCollection :: e -> t
   singletonBuildableCollection e = addBuildableElement e emptyBuildableCollection

   unionBuildableCollections :: t -> t -> t

instance forall e. BuildableCollection [e] e where
   addBuildableElement  = (:)
   emptyBuildableCollection = []
   unionBuildableCollections = (++)

instance forall e hs. (Eq e, Hashable e) => BuildableCollection (HS.HashSet e) e where
   addBuildableElement   = HS.insert
   emptyBuildableCollection  = HS.empty
   unionBuildableCollections  = HS.union

--Example usage:---
testF :: forall t0 e. (Foldable t0, BuildableCollection (t0 e) e) => t0 e
testF = emptyBuildableCollection

class BuildableCollection t e => BuildableUniqueCollection t e  where
   addUniqueBuildableElement  :: e -> t  -> t
   uniqueBuildableCollectionFromList :: [e] -> t

instance forall e . (Eq e ) => BuildableUniqueCollection [e] e where
   addUniqueBuildableElement e list = nub (e: list)
   uniqueBuildableCollectionFromList = nub

instance forall e . (Eq e, Hashable e) => BuildableUniqueCollection (HS.HashSet e) e where
   addUniqueBuildableElement = HS.insert
   uniqueBuildableCollectionFromList = HS.fromList


class (Eq e, BuildableUniqueCollection t e) => AdjustableCollection t e  where
   filterBuildableCollection :: (e -> Bool) -> t -> t
   deleteBuildableElement :: e -> t -> t
   deleteBuildableElement e t = filterBuildableCollection (/= e) t

instance forall e . (Eq e) => AdjustableCollection [e] e where
   filterBuildableCollection = filter

instance forall e . (Eq e, Hashable e) => AdjustableCollection (HS.HashSet e) e where
   filterBuildableCollection = HS.filter
   deleteBuildableElement = HS.delete


class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance forall v. (Show v, Num v) => ToString v where
  toString = show
