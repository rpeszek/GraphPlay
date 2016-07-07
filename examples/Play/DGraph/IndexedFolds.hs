module Play.DGraph.IndexedFolds where

import Data.Hashable
import qualified PolyGraph.DGraph.Indexers as I
import PolyGraph.DGraph
import PolyGraph.DGraph.TreeFold
import qualified Play.DGraph.Types as T
import qualified Play.DGraph.Samples as S (playFirstLast)
import qualified Data.HashSet as HS

-- :: are shown for clarity, not really needed
playGraph :: I.DGraphHelper T.FirstLastWord T.FirstLastLine []
playGraph = I.buidDGraph T.firstLastWordInLine (T.firstLastWordTextLines S.playFirstLast)

playCIndex :: I.CIndexHelper T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) []
playCIndex = I.buildHmCIndex playGraph

-- this counts edges as if graph was expanded to a tree
allImplications ::forall e v . (Hashable v, Eq v) => FoldAccLogic [] v e (HS.HashSet v)
allImplications = FoldAccLogic {
       applyEdge   = const id,
       applyVertex = HS.insert,
       aggregate   = mconcat,
       handleCycle = Right . HS.singleton
    }

playAllImplications:: [String]
playAllImplications =
           map (T.getWordT) . HS.toList $ dfsFold
                         playCIndex
                         (allImplications :: FoldAccLogic [] T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) (HS.HashSet T.FirstLastWord))
                         (head $ I.helperVertices playGraph)

experiments = playAllImplications
