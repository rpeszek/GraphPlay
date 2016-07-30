
module Instances.ListGraphs.DslInterpreter (
  interpretAsNewDiGEdges
  , interpretAsDiGEdgesModification
) where

import PolyGraph.Common (OPair(..))
import qualified PolyForFree.GraphDSL as DSL
import qualified Instances.ListGraphs as ListGraphs

interpretAsNewDiGEdges :: forall v edata . (Eq v) => DSL.GraphDSL v edata  -> ListGraphs.DiEdges v
interpretAsNewDiGEdges program = interpretAsDiGEdgesModification program (ListGraphs.Edges [])

interpretAsDiGEdgesModification :: forall v edata . (Eq v) 
                       => DSL.GraphDSL v edata  -> ListGraphs.DiEdges v -> ListGraphs.DiEdges v
interpretAsDiGEdgesModification program graph = 
            let (_, listOfEdgesWithData) = DSL.runDefaultInterpreter program ([], [])
                newEdges = map (\(v1,v2,_) -> OPair (v1, v2))  listOfEdgesWithData
                oldEdges = ListGraphs.getEdges graph
            in ListGraphs.Edges (oldEdges ++ newEdges)
