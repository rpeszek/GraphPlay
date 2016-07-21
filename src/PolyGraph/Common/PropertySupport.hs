
module PolyGraph.Common.PropertySupport where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import Data.List as L
import PolyGraph.Common.Helpers
import PolyGraph.Instances.ListGraphs

---------------------------------------------
-- list of types used with property tests  --
---------------------------------------------
newtype SimpleUOList v = SimpleUOList { getSimpleUOPs ::[UOPair v]} deriving Show
newtype SimpleOList v  = SimpleOList { getSimpleOPs :: [OPair v]  } deriving Show
newtype MultiUOList v  = MultiUOList { getMultiUOPs :: [UOPair v] } deriving Show
newtype MultiOList v   = MultiOList { getMultiOPs ::   [OPair v]  } deriving Show

newtype VariousVertices v  = VariousVertices { getVs :: [v] } deriving Show
newtype IsolatedVertices v = IsolatedVertices { getIVs:: [v]} deriving Show

data SimpleUOBag v = SimpleUOBag { getSimpleUOMix:: [Either v (UOPair v)] } deriving Show
data SimpleOBag v  = SimpleOBag  { getSimpleOMix::  [Either v (OPair v)]  } deriving Show
data MultiUOBag v  = MultiUOBag  { getMultiUOMix::  [Either v (UOPair v)] } deriving Show
data MultiOBag v   = MultiOBag   { getMultiOMix::   [Either v (OPair v)]  } deriving Show

-----------------------------------------------------------
-- all property data generators implement MixedBag class --
-----------------------------------------------------------
data BagInfo v e = BagInfo {
   getEs:: [e],
   getIsolatedVs:: [v],
   getConnectedVs:: [v]
}

class (Eq v, PairLike e v) => MixedBag b v e where
  getMix :: b -> [Either v e]
  info   :: b -> BagInfo v e
  info   =
           let conv :: ([e],[v],[v]) -> BagInfo v e
               conv (el, ivl, vl) = BagInfo el ivl vl
            in conv . analyzeBag . getMix

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
              foldF (Left v) (edges, isolatedCandiates, connectedVs) =
                       (edges, v : isolatedCandiates, connectedVs)
              foldF (Right e) (edges, isolatedCandiates, connectedVs) =
                       let (v1,v2) = toPair e
                       in (e: edges, isolatedCandiates, v1 : v2 : connectedVs)
              (edgesR, isolatedCandiatesR, connectedVsR) = L.foldr foldF ([],[],[]) vsOrEs
          in  (edgesR, isolatedCandiatesR L.\\ connectedVsR, connectedVsR)

-------------------------------------------------------------------
-- arbitrary instances and MixedBag instances for declared types --
-------------------------------------------------------------------
instance forall v. (Eq v) => MixedBag (SimpleUOList v) v (UOPair v) where
  getMix = L.map(Right) . getSimpleUOPs

instance forall v. (VertexNames v) => Arbitrary(SimpleUOList v) where
  arbitrary = do
    v1Ids <- sublistOf [1..15]
    v2Ids <- sublistOf ([1..15] L.\\ (L.drop 1 v1Ids)) -- v1s and v2s can have at most one element in common
    subset <- scale (*2) $ sublistOf ( [UOPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )
    return $ SimpleUOList subset

instance forall v. (Eq v) => MixedBag (SimpleOList v) v (OPair v) where
  getMix = L.map(Right) . getSimpleOPs

instance forall v. (VertexNames v) => Arbitrary(SimpleOList v) where
  arbitrary = do
    v1Ids <- sublistOf [1..15]
    v2Ids <- sublistOf [1..15]
    subset <- sublistOf ([OPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )
    return $ SimpleOList subset

instance forall v. (Eq v) => MixedBag (MultiOList v) v (OPair v) where
  getMix = L.map(Right) . getMultiOPs

instance forall v . (VertexNames v) => Arbitrary(MultiOList v) where
  arbitrary = do
    SimpleOList uniqueEdges1 <- arbitrary :: Gen (SimpleOList v)
    SimpleOList uniqueEdges2 <- arbitrary :: Gen (SimpleOList v)
    combined <- shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (MultiOList combined)

instance forall v. (Eq v) => MixedBag (MultiUOList v) v (UOPair v) where
  getMix = L.map(Right) . getMultiUOPs

instance forall v . (VertexNames v) => Arbitrary(MultiUOList v) where
  arbitrary = do
    SimpleUOList uniqueEdges1 <- arbitrary :: Gen (SimpleUOList v)
    SimpleUOList uniqueEdges2 <- arbitrary :: Gen (SimpleUOList v)
    combined <- shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (MultiUOList combined)

instance forall v e. (Eq v, PairLike e v) => MixedBag (IsolatedVertices v) v e where
  getMix = L.map(Left) . getIVs

instance forall v. (VertexNames v) => Arbitrary(IsolatedVertices v) where
  arbitrary = do
    vIds <- sublistOf [21..24]
    return (IsolatedVertices (map vName vIds))

instance forall v e. (Eq v, PairLike e v) => MixedBag (VariousVertices v) v e where
  getMix = L.map(Left) . getVs

instance forall v. (VertexNames v) => Arbitrary(VariousVertices v) where
  arbitrary = do
    vIds <- sublistOf ([1..15] ++ [21..24])
    return (VariousVertices (map vName vIds))

--    data SimpleUOBag v = SimpleUOBag { getSimpleUOMix:: [Either v (UOPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (SimpleUOBag v) v (UOPair v) where
  getMix = getSimpleUOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(SimpleUOBag v) where
  arbitrary = do
    uniqueEdges <- arbitrary :: Gen (SimpleUOList v)
    vertices <- arbitrary :: Gen (VariousVertices v)
    let x = getMix vertices :: [Either v (UOPair v)]
    combined <- shuffle ((getMix uniqueEdges ::[Either v (UOPair v)]) ++ (getMix vertices :: [Either v (UOPair v)]))
    return (SimpleUOBag combined)

--    data SimpleOBag v  = SimpleOBag  { getSimpleOMix:: [Either v (OPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (SimpleOBag v) v (OPair v) where
  getMix = getSimpleOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(SimpleOBag v) where
  arbitrary = do
    multiEdges <- arbitrary :: Gen (SimpleOList v)
    vertices <- arbitrary :: Gen (VariousVertices v)
    let x = getMix vertices :: [Either v (OPair v)]
    combined <- shuffle ((getMix multiEdges ::[Either v (OPair v)]) ++ (getMix vertices :: [Either v (OPair v)]))
    return (SimpleOBag combined)

--    data MultiUOBag v  = MultiUOBag  { getMultiUOMix:: [Either v (UOPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (MultiUOBag v) v (UOPair v) where
  getMix = getMultiUOMix

instance forall v . (Eq v, VertexNames v) => Arbitrary(MultiUOBag v) where
  arbitrary = do
    uniqueEdges <- arbitrary :: Gen (MultiUOList v)
    vertices <- arbitrary :: Gen (VariousVertices v)
    let x = getMix vertices :: [Either v (UOPair v)]
    combined <- shuffle ((getMix uniqueEdges ::[Either v (UOPair v)]) ++ (getMix vertices :: [Either v (UOPair v)]))
    return (MultiUOBag combined)

--    data MultiOBag v   = MultiOBag   { getMultiOMix:: [Either v (OPair v)] } deriving Show
instance forall v. (Eq v) => MixedBag (MultiOBag v) v (OPair v) where
  getMix = getMultiOMix

--
instance forall v . (Eq v, VertexNames v) => Arbitrary(MultiOBag v) where
  arbitrary = do
    multiEdges <- arbitrary :: Gen (SimpleOList v)
    vertices <- arbitrary :: Gen (VariousVertices v)
    let x = getMix vertices :: [Either v (OPair v)]
    combined <- shuffle ((getMix multiEdges ::[Either v (OPair v)]) ++ (getMix vertices :: [Either v (OPair v)]))
    return (MultiOBag combined)


-- other arbitrary instances --
instance forall a . (Arbitrary a) => Arbitrary(OPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen a) (arbitrary::Gen a)

instance forall a . (Arbitrary a) => Arbitrary(UOPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen a) (arbitrary::Gen a)
