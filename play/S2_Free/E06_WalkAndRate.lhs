2.06 Free Polymorphism. Combing walk and rating, Composable DSLs using Free-Cofree pattern.
------
This example explores how Free-Cofree pattern makes combing DSLs and Interpreters effortless.  
Continuation of 2.05, it maybe helpful to start there.

This work was motivated by excellent series of posts by [Dave Laing](http://dlaing.org/cofun/posts/free_and_cofree.html),  
and the famous 'Data types a la carte' paper by Wouter Swierstra 
\begin{code}
module  S2_Free.E06_WalkAndRate where
\end{code}

This post is using Free and Cofree tools:
\begin{code}
import Control.Monad.Free
import Control.Comonad.Cofree
import PolyGraph.Common.DslSupport.Coproduct ((:+:), liftLeft, liftRight)
import PolyGraph.Common.DslSupport.Product ((:*:), (*:*))
import PolyGraph.Common.DslSupport.Pairing (Pairing (..))
\end{code}

And an example test program will need:
\begin{code}
import Control.Monad
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List ((\\), zip)
import S1_Cstr.E05_Samples (grid)
import qualified PolyGraph.Buildable.PolyMorth as Morth
import qualified Instances.ListGraphs as ListGraphs
\end{code}

Each lower level language exports: 
 * generating instruction co-products (VWalkInstructions, RatingInstructions)
 * generating interpretations products (VWalkCoinstructions, RatingCoinstructions)
 * free-lifted language instructions if needed in writing new language (e.g. walkTo, history, getRating)
 * container type class representing interpretation state (product type).
 * (optionally, since we can recreate these) language types

All exports from lower level languages (spelled out again):
\begin{code}
import FreeDSL.VWalk (
  VWalkInstructions
  , VWalkDSL  --optional type synonym
  , getNeighbors -- instructions needed to write higher level instruction
  , walkTo
  , history
 )
import FreeDSL.VWalk.CoWalk (
    VWalkCoinstructions
   , buildWalkInstructions
   , VWalkContainer (..)
 )
import S2_Free.E05_Ratings (
    RatingInstructions
  , RatingDSL  
  , findBest  
  , RatingCoinstructions
  , buildRatingInstructions
  , RatingContainer(..) 
 )
import PolyGraph.ReadOnly.Graph (AdjacencyIndex) -- constraint needed by VWalkDSL
\end{code}

The gist of Free-Cofree pattern is in the co-product nature of instruction type, the product 
nature of interpretation type, and the pairing between them. To compose languages we need to compose these by taking sum
of instructions and product of interpretations. Actually, any unconstrained sum type and 
product type would do (like Either and Pair). 
But to be more elegant I use here TypeOperators extension and the naming convention from 
the blog and paper I referred to earlier:
\begin{code}
type RatedWalkInstructions v   = ((VWalkInstructions v)   :+: (RatingInstructions v)) 
type RatedWalkCoInstructions v = ((VWalkCoinstructions v) :*: (RatingCoinstructions v)) 
\end{code}

How cool! We have operators on types. 

If I want to, I can create type synonyms for the combined language (this is optional):
\begin{code}
type RatedWalkDSL v r = Free (RatedWalkInstructions v) r
type RatedWalkInterperter v k = Cofree (RatedWalkCoInstructions v) k
\end{code}

And my DSL and Interpreter are still paired as witnessed by this line of code:
\begin{code}
runPaired :: RatedWalkDSL v r -> RatedWalkInterperter v k -> r
runPaired prog coprog = pair (\_ b -> b) coprog prog
\end{code}

Each lower level language exported interpretation builder method which is really a 'bundled all-in-one interpretation'.  
We need to create such a builder for the new language. This will allow us to distribute the new 
language for further composition.  
As in the previous example, we abstract out the data constructor for the interpretation state
into a type class:
\begin{code}
class (RatingContainer c v, VWalkContainer c v g) => RatedWalkContainer c v g
\end{code}

And we are ready to build new interpreter. Notice the handy multiplication combinator 
that composes both language interpreters!  
\begin{code}
buildRatedWalkInstructions :: forall g v e t c. (RatedWalkContainer c v g, Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g ->  c -> RatedWalkCoInstructions v c
buildRatedWalkInstructions g  = (buildWalkInstructions g) *:* buildRatingInstructions
\end{code}

  > Sidenote: the g parameter is here because of functional type dependency, 
    interpreter needs to keep track of actual graph type,
    but this typically is not needed.   
    About the litany of constraints 
    that both interpreters require: these could have been packaged together but I prefer the
    more explicit approach of just listing them.

The higher level language, we are building, should define and export its own set of commands written using the lower level languages.
It could also be adding its own abstract instructions and interpretations. (But I think this would be a less elegant approach.)
Otherwise, the new language is ready to ship for further composition!
 
To interpret just this language we need to provide container for the interpretation state:
\begin{code}
data ComboContainer v g = ComboContainer {
   vwalkState :: (g, [v]),
   ratingState :: (HM.HashMap v Int)
}
instance RatingContainer (ComboContainer v g) v where
  extractRating = ratingState
  extendRating (ComboContainer wk rg) f = ComboContainer wk (f $ ComboContainer wk rg)
instance VWalkContainer (ComboContainer v g) v g where
  extractWalk = vwalkState
  extendWalk (ComboContainer wk rg) f = ComboContainer (f $ ComboContainer wk rg) rg
\end{code}

And we are done with interpreter work!:
\begin{code}
buildRatedWalkInterpreter :: forall g v e t . (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g -> v -> HM.HashMap v Int -> RatedWalkInterperter v (ComboContainer v g)
buildRatedWalkInterpreter g v map = coiter ((buildWalkInstructions g) *:* buildRatingInstructions) $ ComboContainer (g, [v]) map

interpretRatedWalk :: forall g v e t r. (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          RatedWalkDSL v r -> g -> v -> [(v,Int)] -> r
interpretRatedWalk prog g v ratings = runPaired prog (buildRatedWalkInterpreter g v (HM.fromList ratings))
\end{code}

To write polyglot programs I define a clear set of 'namespaces':
\begin{code}
walkDSL :: VWalkDSL v r -> RatedWalkDSL v r
walkDSL = liftLeft
ratingDSL :: RatingDSL v r -> RatedWalkDSL v r
ratingDSL = liftRight
\end{code}

I will add just one instruction to our higher level language. This is done by writing a 
polyglot program. This program will walk the graph in a way that is informed by ratings.
It is given total number of steps to make. At each step if moves to next highest rated vertex that was 
not visited yet. If it runs out of placed to go it just stops.
The program always returns the full path it traveled at the end.
\begin{code}
ratedWalk :: Eq v => Int -> RatedWalkDSL v [v]
ratedWalk 0  = return []
ratedWalk steps = do
     neighVs <- walkDSL $ getNeighbors
     visited <- walkDSL $ history
     maybeV  <- ratingDSL $ findBest (neighVs \\ visited)
     case maybeV of
        Nothing -> return []
        Just v  -> (walkDSL $ walkTo v) >> (liftM((:) v) . ratedWalk $ (steps-1))
\end{code}

To test, the program will walk unordered grid (that boring grid again, sorry) and try to stay as 
close to the diagonal as possible.  
Writing code to accomplish this, that that does not look like Java, requires some thinking.

The goal is to use abstract vertex type (instead of (Int,Int)).  This way, the type checker works with me. 
the walk.  And we have clearly done it with ratedWalk!
The only thing I need is a good rating function which will place higher rating on the diagonal.
\begin{code}
weightF :: (Int,Int) -> Int
weightF (i,j) = (-1)*(i-j)^2 
\end{code}

Here is a test that walks grid of size 5. It will actually walk diagonally away from (0,0) 
and come back once. The rule of visiting each vertex only once makes it stranded at step 15.
\begin{code}
walkTest n = 
              let graph = grid 5 (,) :: ListGraphs.GEdges (Int, Int)
                  vertices =  Morth.morthToVertices graph  
                  ratings = map weightF vertices
              in interpretRatedWalk (ratedWalk n) graph (0,0) (vertices `zip` ratings)
\end{code}

To see it just execute this in GHCI.
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "Diagonal grid walk looks like this: \n"
  putStrLn $ show (walkTest 40)
\end{code}

**Summary:**  I have not really composed these two languages, they have composed themselves!
There is some minimal boiler plate which is largely copy-paste and probably can be automated. 
Free-Cofree design patter is an eye opening example of what happens in FP when pieces are 
made to fit together.   
Free-Cofree pattern makes writing polyglot programs easy. The question is what is a good use
vs what is an abuse of that pattern.

**Criticism:** The choice of instructions in the presented lower level languages is not very good. My intent
was to demonstrate the concept and nothing more. In particular, RatingDSL mixes concerns of
reading and modifying the rating. Separating these concerns would allow for far more flexibility.
For example, readonly getRating instruction could have been interpreted by simply using a rating 'a -> Int' function
without any need for HashMap.

**Limitations:** 
Problems with writing effectful interpreters.  Any effects that actually
impact state of executing program are probably next to impossible.  Logging and write only
effects should be doable.  I will be searching for alternatives approaches to the Free-Cofree approach that 
keep the DSL-Interpreter pattern going.  Obviously nothing forces my hand to write interpreters as co-monads.
We can write final (lowest level DSL) interpreters using monads and put more code writing effort when composing.
John De A Goes blog (below) is very relevant, so should be stuff on extensible effects.

**As Design Pattern:** 
DSL-Interpreter provides nice isolation of concerns. Probably the most elegant solution is to decompose
the app into a hierarchy of DSLs with higher level DSLs interpreted into lower level DSLs. 
See John De A Goes [A Modern Architecture for FP](http://degoes.net/articles/modern-fp).
