2.08 Free Polymorphism.  DLS for choosing things.  Effectful fold interpreters.
------
This example combines ChoiceDSL from 2.07 with VWalkDSL to create a polyglot guided walk. 
This is a continuation of 2.07, please start by reading that example first.
\begin{code}
module S2_Free.E08_GuidedWalk where
\end{code}

I will be combining these 2 DSLs:
\begin{code}
import FreeDSL.VWalk
import S2_Free.E07_ChoiceDSL
\end{code}
using
\begin{code}
import Control.Monad
import Control.Monad.Free (Free(..))
import PolyGraph.Common.DslSupport.Coproduct ((:<:), (:+:))
\end{code}

Both languages implement polymorphic interpreter type class MInterpreterWithCtx, 
so, the polyglot DSL can be run using interpretInM method (see 2.07).
\begin{code}
import PolyGraph.Common.DslSupport (interpretInM)
\end{code}

VWalkDSL interpreter uses state monad (MonadState constraint) and needs to be passed an instance
of Graph (AdjacencyIndex type class is one of the Graph defining type classes in this library): 
\begin{code}
import Control.Monad.State.Strict (runStateT)
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..))
\end{code}

And to demonstrate final result I will use these graph instances:
\begin{code}
import qualified Instances.ListGraphs as ListGraphs
import S1_Cstr.E05_Samples (bipartiteGraph)
\end{code}

'Hardcoded' type for the polyglot DSL is (see 2.06):
\begin{code}
type GuidedWalkDSL v  = Free ((VWalkInstructions v) :+: (ChoiceInstructions v))
\end{code}

I start with 'atomic' program which asks VWalk DSL what are the choices of neighbors to go to next, asks 
choice DSL which of them to pick and walks to that choice.  I could define it like so:
\begin{code}
guidedStep' ::  forall v.  GuidedWalkDSL v v
guidedStep'  = undefined
\end{code}

However this would hardcode the language and would lock me from using this instruction in even higher
level DSL. So a more polymorphic type is a better choice 
(here f represents any 'bigger' set of polyglot instructions): 
\begin{code}
guidedStep ::  forall v f. ((VWalkInstructions v) :<:f, (ChoiceInstructions v) :<: f) => Free f v
guidedStep  = do
     neighVs <- getNeighbors                        -- WalkDSL
     next    <- choose neighVs                      -- ChoiceDSL
     walkTo next                                    -- WalkDSL
     return next
\end{code}

And I can use the above program to do something more elaborate (walk for specified number of steps
and return a list of visited vertices):
\begin{code}
sampleWalk :: forall v f. ((VWalkInstructions v) :<: f, (ChoiceInstructions v) :<: f) => 
                     Int -> Free f [v]
sampleWalk steps = do
   forM_ [1..steps] (const (guidedStep :: Free f v))  -- polyglot
   walk <- history                                    -- WalkDSL
   return walk
\end{code}

To implement an interpreter that can fold both DSLs at the same time, I need to find a monad stack that works for 
both instances of MInterpreterWithCtx:
```
instance (Show a, Read a, Eq a, MonadIO m, MonadPlus m) => 
                 MInterpreterWithCtx c m (ChoiceInstructions a) where ...
instance (Eq v, AdjacencyIndex g v e t, MonadState ([v]) m) => 
                 MInterpreterWithCtx g m (VWalkInstructions v) where
```
So basically I need a monad m that is both 'MonadState ([v]) m)'' and 'MonadIO m'.
That would be: 'm = StateT [v] IO'.  The interesting bit is that I do not need to spell that out
for GHC,  it figures it out and I can implement interpreter directly skipping State from the signature:

\begin{code}
runGuidedWalkFull :: forall  g v e t r  . (Show v, Read v, Eq v, AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO (r, [v])
runGuidedWalkFull program g v = runStateT (interpretInM g program) [v] -- ([v] is the initial state)

runGuidedWalk :: forall g v e t r . (Show v, Read v, Eq v,  AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO r
runGuidedWalk program g  =  liftM fst . runGuidedWalkFull program g
\end{code}
The other interesting bit to note here is that I did not need to implement GuidedWalkDSL instance 
of MInterpreterWithCtx.  This is because the DslSupport package implements this:

```
instance (MInterpreterWithCtx c m f, MInterpreterWithCtx c m g) => MInterpreterWithCtx c m (f :+: g) where
```
Recursion on types is so cool!!

To test this work you can evaluate testWalk in GHCi and walk bipartite graph using simple CLI interaction
that comes with the Choice DSL:

\begin{code}
testGraph = bipartiteGraph ([0..3], [10..11]) :: ListGraphs.GEdges Int

testWalk :: IO [Int]
testWalk = runGuidedWalk (sampleWalk 3) testGraph 0 
\end{code}

*Limitations, notes, not likes*
 - notice ugly type specification '(:: Free f)' inside the implementation of sampleWalk. GHC errors out without it. 
   Interestingly, there would be no need for that type hint if I have typed both guidedStep and sampleWalk
   using GuidedWalkDSL.  So there is something about the polymorphic type that pushes the limits of type inference. 
   I do not understand that well enough. 
 - if things go wrong GHCi error messages can be misleading (at least to me) when writing polyglot programs and often
   point to a problem with picking overlapping instance of :<: which suggest that there is something wrong with that 
   definition when in reality the problem is elsewhere.

*Remaining questions*
 - The approach in which language instruction type (e.g. ChoiceInstructions) is used to define interpreter type class instance 
   (MInterpreterWithCtx) obviously limits interpreters to only one implementation. 
   This is not elegant and I need to figure out a clean way to specify other interpreters (for example, random choice in Choice DSL).
 - Folding. Need to finally digest "Bananas" paper. Recursion schemes seem to be very relevant.
 - Injections vs bijections.  DSLs are embedded one into the other using type class (:<:) which requires that there is an injection
   from the 'smaller' to the 'bigger' language instruction type. That injection can be reversed to a partial 
   function that is a bijection mapping 'bigger' language instructions to 'smaller' language. 
   It would be cool to figure out how this bijection can be used when interpreting.  It seems very logical that it should be relevant.
 - High performance versions of DLS-interpreter pattern
 - Other alternative approaches invented since "Data types a la carte" paper.
 
