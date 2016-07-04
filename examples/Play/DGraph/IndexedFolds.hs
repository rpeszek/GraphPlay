module Play.DGraph.IndexedFolds where

import PolyGraph.DGraph.Indexers
import PolyGraph.DGraph
import PolyGraph.DGraph.DfsFolds
import Play.DGraph.Types
import Play.DGraph.Samples (playFirstLast)

data MyGraph = MyGraph {
   myEdges    :: [FastDEdge FirstLastLine FirstLastWord],
   myVertices :: [FirstLastWord]
}

instance DGraph(MyGraph) FirstLastWord (FastDEdge FirstLastLine FirstLastWord) [] where
  edges = myEdges
  vertices = myVertices

playFastEdges    = buidFastDEdges emptyFastDEgdes firstLastWordInLine (firstLastWordTextLines playFirstLast) :: [FastDEdge FirstLastLine FirstLastWord]
playFastVertices = fastVertices playFastEdges :: [FirstLastWord]

playGraph = MyGraph {myEdges = playFastEdges, myVertices = playFastVertices}

playCIndex = buildHmCIndex playGraph

-- this counts edges as if graph was expanded to a tree
countEdgesAsOnTree :: ChildTraversingAccLogic [] v e Int
countEdgesAsOnTree = ChildTraversingAccLogic {
       applyEdge   = const (+1),
       applyVertex = const id,
       aggregate   = sum
    }

playEdgeCount:: Int
playEdgeCount = dfsFold playCIndex (countEdgesAsOnTree :: ChildTraversingAccLogic [] FirstLastWord (FastDEdge FirstLastLine FirstLastWord) Int) (head playFastVertices)

experiments = playEdgeCount
