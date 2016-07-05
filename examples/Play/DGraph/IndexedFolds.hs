module Play.DGraph.IndexedFolds where

import qualified PolyGraph.DGraph.Indexers as I
import PolyGraph.DGraph
import PolyGraph.DGraph.DAGFolds
import qualified Play.DGraph.Types as T
import qualified Play.DGraph.Samples as S (playFirstLast)

-- :: are shown for clarity, not really needed
playGraph = I.buidDGraph T.firstLastWordInLine (T.firstLastWordTextLines S.playFirstLast) :: I.DGraphHelper T.FirstLastWord T.FirstLastLine []
playCIndex = I.buildHmCIndex playGraph   :: I.CIndexHelper T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) []

-- this counts edges as if graph was expanded to a tree
countEdgesAsOnTree :: ChildTraversingAccLogic [] v e Int
countEdgesAsOnTree = ChildTraversingAccLogic {
       applyEdge   = const (+1),
       applyVertex = const id,
       aggregate   = sum
    }

playEdgeCount:: Int
playEdgeCount = dfsFold playCIndex (countEdgesAsOnTree :: ChildTraversingAccLogic [] T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) Int) (head $ I.helperVertices playGraph)

experiments = playEdgeCount
