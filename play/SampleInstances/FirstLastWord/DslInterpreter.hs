
module SampleInstances.FirstLastWord.DslInterpreter where
  
import PolyGraph.Common (OPair(..))
import qualified PolyForFree.GraphDSL as DSL
import qualified SampleInstances.FirstLastWord as SentenceGraph

interpretAsFirstLastWordDiGraph :: forall v edata . (Eq v, Show v, Show edata)
                                                      => DSL.GraphDSL v edata  -> SentenceGraph.FLWordText
interpretAsFirstLastWordDiGraph program = interpretAsFirstLastWordDiModification program (SentenceGraph.FLWordText "")

interpretAsFirstLastWordDiModification :: forall v edata . (Eq v, Show v, Show edata) 
                       => DSL.GraphDSL v edata  -> SentenceGraph.FLWordText -> SentenceGraph.FLWordText
interpretAsFirstLastWordDiModification program graph = 
            let (_, listOfEdgesWithData) = DSL.runDefaultInterpreter program ([], [])
                newEdges = map (\(v1,v2,eData) -> show(v1) ++ " " ++ show(eData) ++ " " ++ show(v2) )  listOfEdgesWithData
                oldText = SentenceGraph.getFLWordTextText graph
            in SentenceGraph.FLWordText (oldText ++ "\n" ++ (unlines newEdges))
