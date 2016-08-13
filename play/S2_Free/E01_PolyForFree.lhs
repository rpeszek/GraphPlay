2.01 Free Polymorphism.  Free Decoupling and Polymorphism for free.
------
This example presents decoupling of data production and consumption with benefits similar to 
Examples 1.01 and 1.02 but is accomplished without type classes!  Our plan in a nutshell is:
  - build a graph using a DSL
  - use different interpreters to do different things with it

This approach can accomplish very similar goals to polymorphism:
_write code once and use it across may different types_ and in many ways it goes beyond that.
 
\begin{code}
module S2_Free.E01_PolyForFree (allThisHardWork) where
\end{code}
_Problem - Benefit Statement_:  
Using language provided support for polymorphism (type classes in Haskell or what else have you elsewhere) 
creates a coupling between implementation and consumption code which could be sometimes unwanted.
This becomes more obvious with a strong type systems like Haskell's. 
For example, I have decided that my Graph type classes (such as BuildableGraphDataSet) enforce purity 
and, as result of that decision, I cannot implement a 'self-persisting' graph as an instance. Such decision may
feel wise at some point and may end up to constricting moving forward.

Implementing a DSL and separately writing interpreters which consume its AST resolves these issues.
I can just write bunch of interpreters which are free to do whatever they want!
This think of this as a do-it-yourself handcrafted polymorphism.

  > You can write your code and invoke it too. But, you really do not want to.

That sounds like a lot of tedious and complex work!  Can you do that so it is composable: so 2 DSLs can work together?  

The amazing thing is that it is not hard at all if you do it right and structure it correctly. That correct
structure is called Free Monad. Free monad is structurally a perfect match for both building a DSL and interpreters.
The cost is often one line of code for DSL and one line of code per interpreter per DSL instruction. 

We will be producing a graph using a very simple DSL defined in:
\begin{code}
import qualified FreeDSL.GraphBuilder as DSL
\end{code}

we will consume our program using these interpreters
\begin{code}
import qualified Instances.ListGraphs.DslInterpreter as ListGraphsInterpreter
import qualified SampleInstances.FirstLastWord.DslInterpreter as SentenceGrInterpreter
\end{code}

so we will get to play with specific types defined in
\begin{code}
import qualified Instances.ListGraphs as ListGraphs
import qualified SampleInstances.FirstLastWord as SentenceGraph
\end{code}

The graph we will be working with looks like this:
```
         2
       /   \
0    1       4
       \   /
         3
```

_Production_:  Using DSL forces our code to be agnostic of any type specific stuff.  
\begin{code}
squareGraphProgram :: DSL.GraphDSL Int String
squareGraphProgram = do
  DSL.addVertex 0
  DSL.addEdgeWithData 1 2 "implies"
  DSL.addEdgeWithData 2 4 "maybe implies"
  DSL.addEdgeWithData 1 3 "possibly yields"
  DSL.addEdgeWithData 3 4 "implies"
\end{code}

_Consumption_:
\begin{code}
showProgram :: String
showProgram = DSL.programPrettyShow squareGraphProgram

edgesGraph :: ListGraphs.DiEdges Int
edgesGraph = ListGraphsInterpreter.interpretAsNewDiGEdges squareGraphProgram

sentenceGraph :: SentenceGraph.FLWordText
sentenceGraph = SentenceGrInterpreter.interpretAsFirstLastWordDiGraph squareGraphProgram
\end{code}

And the code to evaluate to see how things work:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "My program:"
    putStrLn showProgram
    putStrLn "interpreted as edge graph:"
    putStrLn $ show edgesGraph
    putStrLn "interpreted as sentences"
    putStrLn $ show sentenceGraph
\end{code}

Notice that 0 vertex has disappeared. This is because both 'destination' types I have picked ignore isolated vertices. 

 
