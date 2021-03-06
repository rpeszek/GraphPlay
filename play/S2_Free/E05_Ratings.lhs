2.05 Free Polymorphism. A DSL that rates things, Composable DSLs using Free-Cofree pattern.
------
My plan is to examine how to design programs by creating composable DSLs and Interpreters.  
This example creates a new DSL, one that has nothing to do with graphs, but will be used
moving forward to demonstrate composability against other DSLs.

This work was motivated by excellent series of posts by [Dave Laing](http://dlaing.org/cofun/posts/free_and_cofree.html).
as well as from the famous 'Data types a la carte' paper by Wouter Swierstra.
\begin{code}
module S2_Free.E05_Ratings (
   allThisHardWork
  , RatingDSL
  , RatingInstructions(..)
  , RatingCoinstructions(..)
  , RatingContainer(..)
  , buildRatingInstructions
  , getRating
  , like
  , findBest
) where
\end{code}

I will use Free monad and comonad (no need to be scared). 
\begin{code}
import Control.Monad.Free
import Control.Comonad.Cofree
import PolyGraph.Common.DslSupport.Pairing (Pairing (..))
import PolyGraph.Common.DslSupport.Coproduct ((:<:), liftDSL)
\end{code}

I will need these for implementation and testing.
\begin{code}
import Control.Monad
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.List (sort)
import qualified Test.QuickCheck as Property
\end{code}
My simple language will allow for rating data elements using 'like element True' 
or 'like element False' calls and will also provide a 'getRating element' method. 
This simplicity can be critiqued as being hacker friendly
(as a single user can do hundreds of likes or dislikes). But I will defend it on the grounds that adding user
checking here would be mixing concerns. There should be a different higher level DSL that handles that. 

To define a language I need a base set of abstract instructions.  Ideally these instructions should
be orthogonal and as atomic as possible.  My instructions are packaged in 'RatingInstructions a r', which is
a _sum type_ (or _coproduct type_). We will rate elements of type 'a' and our program will 
produce results of type 'r':
\begin{code}
data RatingInstructions a r = GetRating a (Int -> r) |
                              Like (a, Bool) r
                                      deriving Functor
\end{code}

On the interpreter side, I also need a base set of abstract 'interpretations' (or co-instructions).  
Notice that RatingCoinstructions is a (OO object like) _product type_.  Type 'a' represents
the things we will be rating. Type 'k' is the internal computation state that interpreter will use
and typically 'r' /= 'k'.  Also notice that, there is a perfect duality between data constructors in 
the RatingInstructions type and record fields in the RatingCoinstructions type (the (,) pair type and
the (->) function type are dual due to the concept of currying): 
\begin{code}
data RatingCoinstructions a k = RatingCoinstructions {
     getRatingCoI  :: a -> (Int, k)
    , likeCoI      :: (a, Bool) -> k  
} deriving Functor
\end{code}

The clear duality between RatingInstructions and RatingCoinstructions types matches the intuition. 
Programs and interpreters are two ends of the same stick, as are base instructions and base 'interpretations'.
This type level duality is extended and formalized by implementing Pairing interface defined in the imported 
DslSupport.Pairing module.  
To understand better the implementation, it helps to know other types that implement
Pairing. Currying provides the duality needed to pair (,) with (->). 
Trivially, Identity type is paired with itself.  This simply delegates to these implementations:
\begin{code}
instance Pairing (RatingCoinstructions v) (RatingInstructions v) where
  pair f (RatingCoinstructions a _) (GetRating x k)  = pair f (a x) k
  pair f (RatingCoinstructions _ a) (Like x k)       = f (a x) k
\end{code}

Free and Cofree are recursive data constructors that expand base instructions and base interpretations
into unbound trees. Best way to think about RatingDSL type as inhabited by all possible, syntactically 
valid sequences for base instructions.  The term 'generating' instructions comes to mind.
\begin{code}
type RatingDSL a r = Free (RatingInstructions a) r 
type RatingInterpreter a k = Cofree (RatingCoinstructions a) k
\end{code}
  > Notice the duality. Programs are Free monads of co-product instruction types.  
    Interpreters are Free co-monads of product 'interpretation' types.  
    Just flip two co-s and we done!

Please notice what the following type signature is witnessing:  _Given program that inhabits 'RatingDSL a r' and interpreter that inhabits 'RatingInterpreter a k' 
we can run the program and get the result_:
\begin{code}
runPaired :: RatingDSL a r -> RatingInterpreter a k -> r
runPaired prog coprog = pair (\_ b -> b) coprog prog
\end{code}
So, the only thing left to do is to define non-abstract language instructions of type 'RatingDSL a r' 
and build interpreter of type 'RatingInterpreter a k'. 

Non-abstract instructions are implemented by simply lifting abstract instruction into the Free tree:
\begin{code}
getRating' ::  a -> RatingDSL a Int
getRating' a   = liftF (GetRating a id)
like'      ::  a -> Bool -> RatingDSL a ()
like' a isGood = liftF (Like (a,isGood) ())
\end{code}

But we can do better than that!  We can define these instructions so they work in any polyglot DSL that
includes RatingDSL instructions. 
This approach is based on 'Data types a la carte' paper.
\begin{code}
getRating ::  forall a polyglot.(Functor polyglot, (RatingInstructions a) :<: polyglot) 
                       => a -> Free polyglot Int
getRating a   = liftDSL $ liftF (GetRating a id)
like      ::  forall a polyglot.(Functor polyglot, (RatingInstructions a) :<: polyglot) 
                       => a -> Bool -> Free polyglot ()
like a isGood = liftDSL $ liftF (Like (a,isGood) ())
\end{code}

A good thing to notice that we can expand the language and implement new instructions, by simply 
writing a program using base instructions (base instructions 'generate' the language):
\begin{code}
findBest :: forall a polyglot.(Functor polyglot, (RatingInstructions a) :<: polyglot) 
                      => [a] -> Free polyglot (Maybe a)
findBest [] = return Nothing
findBest (x0:xs) = do -- todo just is incorrect
    rating0 <- getRating x0
    x1 <- liftM (fromMaybe x0) $ findBest xs
    rating1 <- getRating x1
    return . Just $ bool x0 x1 (rating0 < rating1)
\end{code}
This is it!  We have a language in place and we can start writing programs in it.  But they will not 
do anything other than look pretty (if you like trees) unless we build an interpreter.

Programs are of type 'RatingDSL a r' and each program can choose what 'r' is. This logically stems from
the _coproduct_ nature of instructions.  Interpretations, however, are _product type_ and have exactly the 
opposite requirement. They all need to share the same type to store state when working. 

We will be using 'HashMap a Int' for that state by I want to be more flexible than that. To do that
I am defining a type class that defines valid 'k' for my interpreters. Basically, anything that 
allows me to 'extract' 'HashMap a Int' and change the value of the stored 'HashMap a Int' is valid for me to use.
Note similarity to Comonad type class. 
\begin{code}
class RatingContainer c a where
  extractRating :: c -> (HM.HashMap a Int)
  extendRating :: c -> (c -> (HM.HashMap a Int)) -> c

replaceRating :: (RatingContainer c a) => c -> (HM.HashMap a Int) -> c 
replaceRating c a = extendRating c (const a) 

instance RatingContainer (HM.HashMap a Int) a where
  extractRating = id
  extendRating c f = f c
\end{code}

Now, one by one I need to define how interpretations are handled (reading the implementation
may help in figuring out what is going on):
\begin{code}
coGetRating ::  (RatingContainer c a, Show a, Eq a, Hashable a) => 
                  c -> a -> (Int, c)
coGetRating c a = (HM.lookupDefault 0 a (extractRating c), c) 

coLike :: (RatingContainer c a, Show a, Eq a, Hashable a) => 
                      c -> (a,Bool) -> c
coLike c (a, rating) = 
                    let incr = bool (-1) 1 rating 
                        newMap = HM.insertWith (const (+incr)) a incr (extractRating c)
                    in replaceRating c newMap
\end{code}

And I can now wrap them up together into my product interpretation type:
\begin{code}
buildRatingInstructions :: (RatingContainer c a, Show a, Eq a, Hashable a) =>  
              c -> RatingCoinstructions a c
buildRatingInstructions w = RatingCoinstructions (coGetRating w) (coLike w)
\end{code}

Remember, the only thing left for us to do was to build the interpreter Cofree monad. 
We can do that now by co-iterating the wrapped interpretation:
\begin{code}
buildRatingInterpreter :: (Show a, Eq a, Hashable a) =>  
                             HM.HashMap a Int -> RatingInterpreter a (HM.HashMap a Int)
buildRatingInterpreter initMap = coiter buildRatingInstructions initMap 
\end{code}

That was the last nail, given program and initial set ratings we can run it:
\begin{code}
interpretRating :: (Show a, Eq a, Hashable a) => 
                              RatingDSL a r -> [(a, Int)] -> r
interpretRating prog coState = runPaired prog (buildRatingInterpreter $ HM.fromList coState)
\end{code}

To play with the new language, here is a simple program that: lowers by one the rating on every element it visits if it is
above specified max value, and returns a list of modified elements with new ratings. 
\begin{code}
--adjust values higher than max and return previous highest value larger than max if found
hateOverachievers :: [a] -> Int -> RatingDSL a [(a,Int)]
hateOverachievers list maxRating = do
    overachievers <- filterM (liftM (> maxRating) . getRating) list  
    forM_ overachievers (flip like False)
    mapM (\a -> liftM ((,) a) $ getRating a) overachievers
\end{code}

To test it (and the DSL/Interpreter) I will use the following property (which basically verifies that
the function does what is says it does for a=String):
\begin{code}
prop_ratings :: [(String, Int)] -> Int -> Bool
prop_ratings initRatings maxR = 
      let initNoDupl = HM.toList . HM.fromList $ initRatings
          alist = map(fst) initNoDupl
          expected = map (\(a,i) -> (a,i-1)) . filter( (> maxR) . snd ) $ initNoDupl
          computed = interpretRating (hateOverachievers alist maxR) initNoDupl 
       in sort expected == sort computed
\end{code}

And you can see that things work by evaluating this in GHCI.
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "DLS checks out: \n"
  Property.quickCheck prop_ratings
\end{code}

Notes: So where is the catch?  That does seem to be the best thing since the sliced bread!
The catch is that this uses comonads to build interpreters and comonads are the exact 
opposite of effectful monads.  
So if I want to handle effects in the interpreter, this approach will need some adjustments. 
One approach I should investigate at some point is to use another pairing to flip back to monad on the interpreter side.
Even with current limitation, this approach looks phenomenal. 

More notes in next example. 
