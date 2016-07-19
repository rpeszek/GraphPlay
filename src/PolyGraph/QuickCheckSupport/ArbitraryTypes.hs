
module PolyGraph.QuickCheckSupport.ArbitraryTypes where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Control.Monad
import Data.List as L
import PolyGraph.Common.Helpers
import PolyGraph.Instances.ListGraphs

class VertexNames a where
  vName :: Int -> a

newtype UniqueUOPairs v = UniqueUOPairs { getArbitraryUOPs ::[UOPair v]}
newtype UniqueOPairs v = UniqueOPairs { getArbitraryOPs ::[OPair v]}
newtype IsolatedVertices v = IsolatedVertices { getArbitraryVs:: [v]}

instance forall v. (VertexNames v) => Arbitrary(UniqueUOPairs v) where
  arbitrary = do
    v1Ids <- sublistOf [1..15]
    v2Ids <- sublistOf ([1..15] L.\\ (L.drop 1 v1Ids)) -- v1s and v2s can have at most one element in common
    return (UniqueUOPairs $ [UOPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )

-- 1,3,5,6   1,5,9,10
instance forall v. (VertexNames v) => Arbitrary(UniqueOPairs v) where
  arbitrary = do
    v1Ids <- sublistOf [1..15]
    v2Ids <- sublistOf [1..15]
    return (UniqueOPairs $ [OPair(vName v1Id, vName v2Id) | v1Id <- v1Ids, v2Id <- v2Ids] )

instance forall v. (VertexNames v) => Arbitrary(IsolatedVertices v) where
  arbitrary = do
    vIds <- sublistOf [21..24]
    return (IsolatedVertices (map vName vIds))

-- instances for types defined elsewere --
instance forall v e. (VertexNames v) => Arbitrary(Vertices v e) where
  arbitrary = do
    vIds <- sublistOf ([1..15] ++ [21..24])
    return (Vertices (map vName vIds))

instance forall a . (Arbitrary a) => Arbitrary(OPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen a) (arbitrary::Gen a)

instance forall a . (Arbitrary a) => Arbitrary(UOPair a) where
  arbitrary = liftM2 (\a b -> fromPair (a,b)) (arbitrary::Gen a) (arbitrary::Gen a)

instance forall v . (VertexNames v) => Arbitrary(Edges v (OPair v)) where
  arbitrary = do
    UniqueOPairs uniqueEdges1 <- arbitrary :: Gen (UniqueOPairs v)
    UniqueOPairs uniqueEdges2 <- arbitrary :: Gen (UniqueOPairs v)
    combined <- shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (Edges combined)

instance forall v . (VertexNames v) => Arbitrary(Edges v (UOPair v)) where
  arbitrary = do
    UniqueUOPairs uniqueEdges1 <- arbitrary :: Gen (UniqueUOPairs v)
    UniqueUOPairs uniqueEdges2 <- arbitrary :: Gen (UniqueUOPairs v)
    combined <- shuffle (uniqueEdges1 ++ uniqueEdges2)
    return (Edges combined)
