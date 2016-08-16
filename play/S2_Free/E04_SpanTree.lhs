2.03 Free Polymorphism.  Spanning Tree, Computing on DSLs.
------
Typically, people write DSLs to compute in them or with them. In this example I will compute *on* a DSL.  

Undirected graph traversals define subgraphs called _spanning trees_.
[Wikipedia](https://en.wikipedia.org/wiki/Spanning_tree) shows spanning tree for a grid graph.

This example computes spanning tree for BFS traversal (Breadth-first Search, see 2.02) of any undirected graph.  
The plan is to 
   * use VTraversal DSL (examples 2.02 and 2.03) to observe BFS, 
   * __map__ these observations to GraphDSL programs (example 2.01) 
   * threat these programs as ordinary data elements and __reduce__ thus list into one long program.

The reduced program will build BFS spanning tree and will offer polymorphic choice of input and output types. 

The motivation for this example is to show one of many ways 2 DSLs can interact.  I could have just used 
VTraversal DSL combined with graph creating (@~>@) or graph adjustment methods available in GraphPlay library
but then I would have missed out on a perfect opportunity to compute __on__ programs not just _with_ programs.

\begin{code}
module S2_Free.E04_SpanTree where
\end{code}

I will be combining 2 different DSLs:
\begin{code}
import FreeDSL.BFS.VTraversal
import qualified FreeDSL.BFS.Interpreter as BfsInterpreter
import qualified FreeDSL.GraphBuilder as GraphBuilderDSL
import qualified Instances.ListGraphs.DslInterpreter as BuilderIntereter
\end{code}

I will need these to help in program spelling:
\begin{code}
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Loops
\end{code}

And will use these to demonstrate the results:
\begin{code}
import S1_Cstr.E05_Samples (grid)
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property
\end{code}

Technically, spanning tree is a subgraph of the input graph but from computing stand point it is best
to treat is as a separate structure since it has natural direction (it is a tree).

I did not create a separate DSL for building Trees, instead I will repurpose existing one 
(GraphDSL from example 2.01).  
My DSL has type TreeBuildingLang v and just one instruction:
\begin{code}
type TreeBuildingLang v = GraphBuilderDSL.GraphDSL v ()

addNodeInstruction :: v -> v -> TreeBuildingLang v
addNodeInstruction parent newNode = GraphBuilderDSL.addEdgeWithData parent newNode ()
\end{code}

I will use 2 simple programs written in this DSL:
\begin{code}
noOpProg :: TreeBuildingLang v
noOpProg = 
          do 
            return ()

addSingleNodeProg :: (v,v) -> TreeBuildingLang v
addSingleNodeProg (parent,newNode) = 
         do 
           addNodeInstruction parent newNode
\end{code}

_Understanding the following code:_  
In the type definition 'VTraversal a v r', 
'a' represents internal state the program can use and 'r' is the program final result type.
Thus, to implement spanningTree type I need to write in VTraversal DSL and return a program that can be run 
in TreeBuildingLang language interpreter.
\begin{code}
spanningTree :: (Eq v) => v -> VTraversal [(v,v)] v (TreeBuildingLang v)
spanningTree root = do
  -- collect traversal observations into a list of node pairs
  rootWithAnnotation root []
  whileJust_ (nextAsVertexPair)
      ((modifyAnnotationAt root) . (:) )
  traversedNodes <- (liftM fromJust) . getAnnotationAt $ root

  -- map that list to a list of simple programs, each adding single node to the tree 
  listOfProgs <- mapM ((liftM addSingleNodeProg) . return) traversedNodes
  -- reduce the list of programs into one long program concatenating all instructions
  foldM (\composedProg simpleProg ->  return $ composedProg >> simpleProg) noOpProg listOfProgs 
\end{code}
  
  > _Sidenote:_ If you new to monads, the __reduce__ step could use some explanation. Think about 
    'algebra operations on programs':  
    prog1 >>= prog2 is a program that first runs prog1 and then uses its output to run prog2  
    prog1 >> prog2 is a program that runs prog1 ignores its output and runs prog2.  
    Since my TreeBuildingLang is a very simple language, 
    prog1 >> prog2 simply means to append all instructions from prog2
    to the end of prog 1.  
    >> and >>= are not defined as part of DSL, they are generic way of composing computations 
    and are part of Haskell base library.
    How cool, and probably the best way to think about monads too!

I will use grid graph (again, sorry) of size 5 to demonstrate the results:
\begin{code}
type Point = (Int, Int)
myGraph = grid 5 (,) :: ListGraphs.GEdges Point
\end{code}

Both 'VTraversal a v r' and 'TreeBuildingLang v' types represent only language AST-ies. 
They do nothing on their own, they both need a language interpreter. 

We need to interpret the 'outside' DSL (VTraversal) layer before we can interpret 'TreeBuildingLang'. 
We can make polymorphic choices similar to the choices we had in examples 2.01 - 03. 
Here I just assume that my input has type ListGraphs.GEdges (which is undirected graph type) 
and the output is ListGraphs.DiEdges Point (a directed graph type). 
\begin{code}
runSpanningTree :: Point -> (ListGraphs.GEdges Point) -> (ListGraphs.DiEdges Point)
runSpanningTree from graph = BuilderIntereter.interpretAsNewDiGEdges $ 
                               BfsInterpreter.runBFS (spanningTree from) graph
\end{code}

You can evaluate the following in ghci to see the outcome of my hard work:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "Distance from 00 to 55 is: \n"
  putStrLn $ show $ runSpanningTree (0,0) myGraph
\end{code}

Note a code purity problem with the spanningTree code. 
It is not possible for fromJust to throw exception in that code since the value it converts 
cannot possibly be Nothing but the fact that I had to use fromJust bugs me.  I would prefer to have my types
handle it. I am ignoring this issue for now. 

Haskell and monads are sooo deep and powerful.  
This and previous example are to me what makes any OO programming feel inferior. 
