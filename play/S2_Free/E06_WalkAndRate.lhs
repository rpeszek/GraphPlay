2.05 Free Polymorphism. Combing walk and rating, Composable DSLs using Free-Cofree pattern.
------
This example explores how Free-Cofree pattern makes combing DSLs and Interpreters effortless.

This work was motivated by excellent series of posts by [Dave Laing](http://dlaing.org/cofun/posts/free_and_cofree.html),  
the famous 'Data types a la carte' paper by Wouter Swierstra 
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

Each language exports: 
 * abstract instructions co-products (VWalkInstructions, RatingInstructions)
 * abstract interpretations products (VWalkCoinstructions, RatingCoinstructions)
 * free-lifted language instructions (e.g. walkTo, history, getRating)
 * container type class representing interpretation state (product type).
 * (optionally, since we can recreate these) language types

And this is all that DSLs need to export to compose them into higher level language
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
import PolyGraph.ReadOnly.Graph (AdjacencyIndex)
\end{code}

The gist of Free-Cofree pattern is in the co-product nature of instruction type and product 
nature of interpretation type. To compose languages we need to compose these by taking sum
of instructions and product of interpretations. Actually, any unconstrained sum type and 
product type would do (like Either and Pair). 
But to be more elegant I use here TypeOperators extension and naming convention from 
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

Each language provided bundled interpretations (or co-instructions) using a builder method. 
We need to create a builder for the new language. This will allow us distribute the new 
language as well. As before we abstract out the data constructor for interpretation state
into a type class:
\begin{code}
class (RatingContainer c v, VWalkContainer c v g) => RatedWalkContainer c v g
\end{code}

And we are ready to build new interpreter co-product. Notice handy combinator 
that composes both language interpreters!  
\begin{code}
buildRatedWalkInstructions :: forall g v e t c. (RatedWalkContainer c v g, Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g ->  c -> RatedWalkCoInstructions v c
buildRatedWalkInstructions g  = (buildWalkInstructions g) *:* buildRatingInstructions
\end{code}

  > Sidenote: the g parameter is here because of functional type dependency, 
    interpreter needs to keep track of actual graph type
    but this typically would not be needed. Another note is about the litany of constraints 
    that both interpreters require, these could have been packaged together but I prefer the
    more explicit approach of just listing them.

The combined language may define its own set of commands on top of combined languages or 
by adding its own abstract instructions and interpretations. See discussion at the end of this example.
Otherwise the new language is ready to ship for further composition.
 
To just interpret this language we need to provide container for interpretation state:
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

And we are done!:
\begin{code}
buildRatedWalkInterpreter :: forall g v e t . (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g -> v -> HM.HashMap v Int -> RatedWalkInterperter v (ComboContainer v g)
buildRatedWalkInterpreter g v map = coiter ((buildWalkInstructions g) *:* buildRatingInstructions) $ ComboContainer (g, [v]) map

interpretRatedWalk :: forall g v e t r. (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          RatedWalkDSL v r -> g -> v -> [(v,Int)] -> r
interpretRatedWalk prog g v ratings = runPaired prog (buildRatedWalkInterpreter g v (HM.fromList ratings))
\end{code}

To write polyglot programs we need to have a nice and clear 'namespace':
\begin{code}
walkDSL :: VWalkDSL v r -> RatedWalkDSL v r
walkDSL = liftLeft
ratingDSL :: RatingDSL v r -> RatedWalkDSL v r
ratingDSL = liftRight
\end{code}

As an example I will add a new instruction to the new language (I will implement a 
polyglot program). This program walks the graph in a way that is informed by ratings.
It is given total number of steps to make. At each step if moves to next highest rated vertex.
It visits each vertex at most once and if it runs out of placed to go it just stops.
Always returns the full path it traveled at the end.
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

As example I will walk unordered grid (grid again, sorry) and try to stay as 
close to the diagonal as possible. Writing code to accomplish this,  
that that does not look like Java/other OO requires some thinking.

The goal is to have type checker work with me and not use (Int,Int) coordinates when navigating
the walk.  And we have clearly done it with ratedWalk!
The only thing I need is a good rating function which will place higher rating on the diagonal.
\begin{code}
weightF :: (Int,Int) -> Int
weightF (i,j) = (-1)*(i-j)^2 
\end{code}

Here is a test that walks grid of size 5. It will actually walk diagonally away from (0,0) 
and come back once. The rule of not repeating previous steps makes it stranded at step 15.
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
Free-Cofree pattern makes writing polyglot programs easy. The question is what is the right
use and what is abuse of that pattern.

**Criticism:** The choice of instructions for presented languages is not very good. My intent
was to demonstrate the concept and nothing more. In particular, RatingDSL mixes concerns of
reading and modifying the rating. Separating these concerns would allow for far more flexibility.
For example, readonly getRating command could have been interpreted by simply using a rating a -> Int function.

**Limitations:** 
Problems with writing effectful interpreters one big issue.  Any effects that actually
impact state of executing program are probably next to impossible.  Logging and write only
effects should be doable.  I will be searching for alternatives approaches to the DSL-Interpreter
pattern.

**Solutions:** 
Two ideas I know about on high level are:  think on interpreting a program in DSL1 as implementing
instructions used in that program using a collection of lower level DSLs.  Effectively this becomes an
injection from higher level abstract instruction set into Free co-product of lower level instructions.
That injection has quite a bit of structure (it extends to a natural transformation on HASK). 
There is a FP machinery that can help and I need to study it.

By chaining DSL implementations we we end up with a big co-product of low level instructions
We can then implement that co-product using effectful Monads. Having to work with a big co-product 
looks like a large task but, I think, it is linear and doable.  

The second approach: extensible effects. There is Haskell library for it that would be cool to study.  

And also, there are the transformer options of stacking monads one on top of the other. That works
with monadic interpreters which just unstack.

**As Design Pattern:** 
DSL-Interpreter provides nice isolation of concerns which gets even better if DSL layering (see above)
is part of the design.  Quite possibly, any App could be decomposed by implementing low level DSLs 
and writing very short single minded programs that define higher and higher level of DSLs.
See John De A Goes [A Modern Architecture for FP](http://degoes.net/articles/modern-fp).
