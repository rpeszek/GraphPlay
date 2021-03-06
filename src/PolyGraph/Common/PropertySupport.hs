
module PolyGraph.Common.PropertySupport (
  SimpleUOList(..)
  , SimpleOList(..)
  , MultiUOList(..)
  , MultiOList(..)
  , VariousVertices(..)
  , IsolatedVertices(..)
  , SimpleUOBag(..)
  , SimpleOBag(..)
  , MultiUOBag(..)
  , MultiOBag(..)
  , MixedBag (..)
  , VertexNames (..)
  , analyzeBag
  , trippleCounts
  , (&&&)
  -- , bijectionCheckHelper
  -- , isBijection
) where

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import qualified Test.QuickCheck.Gen as Gen
import Control.Monad (liftM2)
import Data.List as L
import PolyGraph.Common (UOPair(..), OPair(..), PairLike(toPair, fromPair))
import PolyGraph.ReadOnly as Base

---------------------------------------------------
-- list of 'MixedBags' used with property tests  --
---------------------------------------------------
data SimpleUOBag v = SimpleUOBag { getSimpleUOMix:: [Either v (UOPair v)] } deriving (Show, Read)
data SimpleOBag v  = SimpleOBag  { getSimpleOMix::  [Either v (OPair v)]  } deriving (Show, Read)
data MultiUOBag v  = MultiUOBag  { getMultiUOMix::  [Either v (UOPair v)] } deriving (Show, Read)
data MultiOBag v   = MultiOBag   { getMultiOMix::   [Either v (OPair v)]  } deriving (Show, Read)

---------------------------------------------
-- helper types, more specialized bags     --
---------------------------------------------
newtype SimpleUOList v = SimpleUOList { getSimpleUOPs ::[UOPair v]} deriving Show
newtype SimpleOList v  = SimpleOList { getSimpleOPs :: [OPair v]  } deriving Show
newtype MultiUOList v  = MultiUOList { getMultiUOPs :: [UOPair v] } deriving Show
newtype MultiOList v   = MultiOList { getMultiOPs ::   [OPair v]  } deriving Show

newtype VariousVertices v  = VariousVertices { getVs :: [v] } deriving Show
newtype IsolatedVertices v = IsolatedVertices { getIVs:: [v]} deriving Show

-----------------------------------------------------------
-- all property data generators implement MixedBag class --
-----------------------------------------------------------
class (Eq v, PairLike e v) => MixedBag b v e where
  getMix :: b -> [Either v e]
  analyze :: b -> ([e],[v],[v])
  analyze = analyzeBag . getMix

---------------------------------------------------------------------------------
-- VertexNames is used to define Int based property test data of various types --
---------------------------------------------------------------------------------
class VertexNames a where
  vName :: Int -> a

instance VertexNames(Int) where
  vName = id
instance VertexNames(String) where
  vName i = "v"++show(i)


----------------------
-- helper functions --
----------------------
-- | splits bag getMix list into edges, insolatedVertices and connectedVertices
analyzeBag :: forall v e . (Eq v, PairLike e v) =>  [Either v e] -> ([e],[v],[v])
analyzeBag vsOrEs =
          let foldF :: Either v e -> ([e],[v],[v]) -> ([e],[v],[v])
              foldF (Left v) (bedges, isolatedCandiates, connectedVs) =
                       (bedges, v : isolatedCandiates, connectedVs)
              foldF (Right e) (bedges, isolatedCandiates, connectedVs) =
                       let (v1,v2) = toPair e
                       in (e: bedges, isolatedCandiates, v1 : v2 : connectedVs)
              (edgesR, isolatedCandiatesR, connectedVsR) = L.foldr foldF ([],[],[]) vsOrEs
              uniqueConnectedVs = nub connectedVsR
              uniqueIsolatedVs = nub isolatedCandiatesR
          in  (edgesR, nub $ uniqueIsolatedVs L.\\ uniqueConnectedVs, uniqueConnectedVs)

trippleCounts :: forall a b c . ([a],[b],[c]) -> (Int, Int, Int)
trippleCounts (as, bs, cs) = (L.length as, L.length bs, L.length cs)

(&&&) ::  (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&) = liftM2 . liftM2 $ (&&)

-- this is fine only for vertices
bijectionCheckHelper :: forall a1 a2 . (Eq a1, Eq a2) =>
                                [a1] -> (a1 -> a2) -> Bool
bijectionCheckHelper testData f =
                     let  inputs = L.nub testData
                          values = map f inputs
                     in L.length values == L.length inputs

-- | this one is interesting it should basically check edge counts
-- TODO this is not ready
isBijection :: forall v0 e0 v1 e1 . (Eq v0, Eq v1 )=>
                      [v0] -> [e0] -> Base.GMorphism v0 e0 v1 e1 -> Bool
isBijection  = undefined



-------------------------------------------------------------------
-- arbitrary instances and MixedBag instances for declared types --
-------------------------------------------------------------------
instance forall v. (Eq v) => MixedBag (SimpleUOList v) v (UOPair v) where
  getMix = L.map(Right) . getSimpleUOPs

instance forall v. (VertexNames v) => Arbitrary(SimpleUOList v) where
  arbitrary = do
    v1Ids <- Gen.sublistOf [1..15]
    v2Ids <- Gen.sublistOf ([1..15] L.\\ (L.drop 1 v1Ids)) -- v1s and v2s can have at most one element in common
    subset <- Gen.scale (*2) $ Gen.sublistOf ( [UOPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )
    return $ SimpleUOList subset

instance forall v. (Eq v) => MixedBag (SimpleOList v) v (OPair v) where
  getMix = L.map(Right) . getSimpleOPs

instance forall v. (VertexNames v) => Arbitrary(SimpleOList v) where
  arbitrary = do
    v1Ids <- Gen.sublistOf [1..15]
    v2Ids <- Gen.sublistOf [1..15]
    subset <- Gen.sublistOf ([OPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )
    return $ SimpleOList subset

instance forall v. (Eq v) => MixedBag (MultiOList v) v (OPair v) where
  getMix = L.map(Right) . getMultiOPs

instance forall v . (VertexNames v) => Arbitrary(MultiOList v) where
  arbitrary = do
    SimpleOList uniqueEdges1 <- arbitrary :: Gen.Gen (SimpleOList v)
    SimpleOList uniqueEdges2 <- arbitrary :: Gen.Gen (SimpleOList v)
    combined <- Gen.shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (MultiOList combined)

instance forall v. (Eq v) => MixedBag (MultiUOList v) v (UOPair v) where
  getMix = L.map(Right) . getMultiUOPs

instance forall v . (VertexNames v) => Arbitrary(MultiUOList v) where
  arbitrary = do
    SimpleUOList uniqueEdges1 <- arbitrary :: Gen.Gen (SimpleUOList v)
    SimpleUOList uniqueEdges2 <- arbitrary :: Gen.Gen (SimpleUOList v)
    combined <- Gen.shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (MultiUOList combined)

instance forall v e. (Eq v, PairLike e v) => MixedBag (IsolatedVertices v) v e where
  getMix = L.map(Left) . getIVs

instance forall v. (VertexNames v) => Arbitrary(IsolatedVertices v) where
  arbitrary = do
    vIds <- Gen.sublistOf [21..24]
    return (IsolatedVertices (map vName vIds))

instance forall v e. (Eq v, PairLike e v) => MixedBag (VariousVertices v) v e where
  getMix = L.map(Left) . getVs

instance forall v. (VertexNames v) => Arbitrary(VariousVertices v) where
  arbitrary = do
    vIds <- Gen.sublistOf ([1..15] ++ [21..24])
    return (VariousVertices (map vName vIds))

--    data SimpleUOBag v = SimpleUOBag { getSimpleUOMix:: [Either v (UOPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (SimpleUOBag v) v (UOPair v) where
  getMix = getSimpleUOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(SimpleUOBag v) where
  arbitrary = do
    uniqueEdges <- arbitrary :: Gen.Gen (SimpleUOList v)
    vertices <- arbitrary :: Gen.Gen (VariousVertices v)
    --let x = getMix vertices :: [Either v (UOPair v)]
    combined <- Gen.shuffle ((getMix uniqueEdges ::[Either v (UOPair v)]) ++ (getMix vertices :: [Either v (UOPair v)]))
    return (SimpleUOBag combined)

--    data SimpleOBag v  = SimpleOBag  { getSimpleOMix:: [Either v (OPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (SimpleOBag v) v (OPair v) where
  getMix = getSimpleOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(SimpleOBag v) where
  arbitrary = do
    multiEdges <- arbitrary :: Gen.Gen (SimpleOList v)
    vertices <- arbitrary :: Gen.Gen (VariousVertices v)
    --let x = getMix vertices :: [Either v (OPair v)]
    combined <- Gen.shuffle ((getMix multiEdges ::[Either v (OPair v)]) ++ (getMix vertices :: [Either v (OPair v)]))
    return (SimpleOBag combined)

--    data MultiUOBag v  = MultiUOBag  { getMultiUOMix:: [Either v (UOPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (MultiUOBag v) v (UOPair v) where
  getMix = getMultiUOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(MultiUOBag v) where
  arbitrary = do
    uniqueEdges <- arbitrary :: Gen.Gen (MultiUOList v)
    vertices <- arbitrary :: Gen.Gen (VariousVertices v)
    --let x = getMix vertices :: [Either v (UOPair v)]
    combined <- Gen.shuffle ((getMix uniqueEdges ::[Either v (UOPair v)]) ++ (getMix vertices :: [Either v (UOPair v)]))
    return (MultiUOBag combined)

--    data MultiOBag v   = MultiOBag   { getMultiOMix:: [Either v (OPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (MultiOBag v) v (OPair v) where
  getMix = getMultiOMix

--
instance forall v . (Eq v, VertexNames v) => Arbitrary(MultiOBag v) where
  arbitrary = do
    multiEdges <- arbitrary :: Gen.Gen (SimpleOList v)
    vertices <- arbitrary :: Gen.Gen (VariousVertices v)
    --let x = getMix vertices :: [Either v (OPair v)]
    combined <- Gen.shuffle ((getMix multiEdges ::[Either v (OPair v)]) ++ (getMix vertices :: [Either v (OPair v)]))
    return (MultiOBag combined)


-- other arbitrary instances --
instance forall a . (Arbitrary a) => Arbitrary(OPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen.Gen a) (arbitrary::Gen.Gen a)

instance forall a . (Arbitrary a) => Arbitrary(UOPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen.Gen a) (arbitrary::Gen.Gen a)
