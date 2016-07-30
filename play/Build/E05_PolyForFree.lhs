
Strictly speaking this is not polymorphism, but it does accomplish very similar goals of writing code 
once and using it across may different types. 
 
\begin{code}
module Build.E05_PolyForFree where
\end{code}
\begin{code}
import qualified PolyForFree.GraphDSL as DSL
import qualified Instances.ListGraphs as ListGraphs
import qualified SampleInstances.FirstLastWord as SentenceGraph

import qualified Instances.ListGraphs.DslInterpreter as ListGraphsInterpreter
import qualified SampleInstances.FirstLastWord.DslInterpreter as SentenceGrInterpreter

squareGraphProgram :: DSL.GraphDSL Int String
squareGraphProgram = do
  DSL.addVertex 0
  DSL.addEdgeWithData 1 2 "implies"
  DSL.addEdgeWithData 2 4 "maybe implies"
  DSL.addEdgeWithData 1 3 "possibly yields"
  DSL.addEdgeWithData 3 4 "implies"

showProgram :: String
showProgram = DSL.programPrettyShow squareGraphProgram

edgesGraph :: ListGraphs.DiEdges Int
edgesGraph = ListGraphsInterpreter.interpretAsNewDiGEdges squareGraphProgram

sentenceGraph :: SentenceGraph.FLWordText
sentenceGraph = SentenceGrInterpreter.interpretAsFirstLastWordDiGraph squareGraphProgram

allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "My program:"
    putStrLn showProgram
    putStrLn "interpreted as edge graph:"
    putStrLn $ show edgesGraph
    putStrLn "interpreted as sentences"
    putStrLn $ show sentenceGraph
\end{code}
