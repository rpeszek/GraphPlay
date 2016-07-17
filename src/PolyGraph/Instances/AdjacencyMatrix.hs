{-
  Implementation of a arbitrary di-graph as its ajacency matrix
  Assumes Ord v for simplicity and reasonable performance.
-}
module PolyGraph.Instances.AdjacencyMatrix where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Adjustable.GDSAdjust
import PolyGraph.Common.Helpers
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as S
import Data.Matrix as M
import PolyGraph.Instances.EdgeCountMapGraph


type DiAdjacencyMatrix v = EdgeCountMap v (OPair v)
type AdjacencyMatrix v   = EdgeCountMap v (UOPair v)

-- TODO text packing
prettyAdjacencyMatrix :: forall v e . (Show v, Eq v, Eq e, Hashable e, Ord v, PairLike e v) => EdgeCountMap v e -> String
prettyAdjacencyMatrix g =  let  vertices :: S.Seq v
                                vertices = S.sort . S.fromList . HS.toList $ (getVertices g)
                                vertexDisplay = show vertices
                                matrixDisplay = M.prettyMatrix (getAdjacencyMatrix vertices g)
                           in  "Vertices " ++ vertexDisplay ++ "\n" ++ "Adjacenty Matrix " ++ matrixDisplay

getOrdAdjacentMatrix :: forall v e . (Eq v, Ord v, Eq e, Hashable e, PairLike e v) =>
                                       EdgeCountMap v e -> M.Matrix Int
getOrdAdjacentMatrix g = getAdjacencyMatrix (S.sort . S.fromList . HS.toList $ (getVertices g)) g

getAdjacencyMatrix :: forall v e. (Eq v, Ord v, Eq e, Hashable e, PairLike e v) =>
                                       S.Seq v -> EdgeCountMap v e -> M.Matrix Int
getAdjacencyMatrix orderedVertices g =
       let size = length orderedVertices
           genFunction :: (Int, Int) -> Int
           genFunction rc = let (r,c) = rc
                                v1 = orderedVertices `S.index` (r -1)
                                v2 = orderedVertices `S.index` (c -1)
                            in HM.lookupDefault 0 (fromPair (v1,v2)) (getMap g)
       in M.matrix size size genFunction
