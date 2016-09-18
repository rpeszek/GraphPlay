2.07 Free Polymorphism.  DLS for choosing things.  Effectful fold interpreters.
------

This example builds another DSL that has nothing to do with Graphs but will be useful moving forward. 
The goal is to examine 'Data Types a la carte' approach in both DSL and interpreter design. 
In this approach interpreters are Free AST tree folds and can be effectful. (Catamorphisms.)
\begin{code}
module S2_Free.E07_ChoiceDSL where

import Control.Monad (MonadPlus, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free (Free(..), liftF)
import PolyGraph.Common.DslSupport (MInterpreterWithCtx (..), interpretInM)
import PolyGraph.Common.DslSupport.Coproduct ((:<:), liftDSL)
\end{code}

This example uses a very simple DSL with just one abstract instruction.
That instruction knows how to pick element from a list.
\begin{code}
data ChoiceInstructions a r =  Choose [a] (a -> r) deriving Functor
\end{code}

Here is type synonym defining new DSL type (not really needed for polyglot programs):
\begin{code}
type ChoiceDSL a r = Free (ChoiceInstructions a) r
\end{code}

The actual choose instruction is defined polymorphically on unspecified 'bigger' language (as I have done in 2.05):
\begin{code}
choose ::  forall a polyglot. (Functor polyglot, (ChoiceInstructions a) :<: polyglot) 
                         => [a] -> Free polyglot a
choose list =  liftDSL $ liftF (Choose list id)
\end{code}

My example interpreter will simply ask user what to pick using a primitive CLI interaction.  
The following code simply pattern matches on the expressions, asks user to make a pick, and recursively processes next language instruction.  
The 'or' nature of coproduct instruction set would allow me to equally easy pattern match on a larger list of abstract instructions
as long as I know all of them.  
\begin{code}
interpretChoices :: (Show a, Read a, Eq a) => ChoiceDSL a r -> IO r
interpretChoices (Free (Choose alist nF)) = do
      putStrLn $ "Make a pick " ++ show(alist)
      choiceStr <- getLine
      let choiceA = read choiceStr
      guard (elem choiceA alist)
      interpretChoices $ nF choiceA
interpretChoices (Pure r) = return r
 
testChoiceProg :: Int -> [a] -> ChoiceDSL a [a]
testChoiceProg noPicks alist = do
   mapM (const $ choose alist) [1 .. noPicks]

runTestChoice :: IO [Int]
runTestChoice = interpretChoices $ testChoiceProg 3 [0..10]
\end{code}

To work with polyglot DSLs I need to do better. 
The idea is to think about interpretation as folding of program instructions to produce result. 
This approach splits interpreter logic into fold and accumulation (often called algebra).    
It turns about that the following fold function is easy to implement 
(think of 'f' as 'DslInstructions' type, see DslSupport module for implementation):
```
foldDslProg :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b 
```

I should be easily able to pattern match (like above) to implement accumulation step: 
```
interpretStep :: f (IO a) -> IO a
```
  > I use the term 'Step' as an analogy to fold.  This is not a very precise analogy.
    A better way of thinking about it maybe evaluation of the outer most language expression.  
    The term algebra is probably better to use. 
 
and I have IO interpreter!
```
interpretInIO :: Free f a -> IO a
interpretInIO prog = foldDslProg return interpretInIO prog
```

But that is still not good enough. What is left do to is to generalize 'interpretStep' to work with other things than IO and to make it 
polymorphic to work with any polyglot DSL instructions 'f'.  All my Graph DSLs, shown in previous examples, are agnostic
of which graph type is used.  It is interpreter job to worry about stuff like that. 
So, in general, effectful interpreter step needs to also be able to handle additional context 'c'.  
Generalize polymorphic constraint for interpretStep is defined in DslSupport module as: 
```
class (Monad m, Functor f) => MInterpreterWithCtx c m f where
  interpretStepM :: c -> f (m a) -> m a
```
And we can implement that general constraint that will allow us to use any monad stack that includes IO:
\begin{code}
instance (Show a, Read a, Eq a, MonadIO m, MonadPlus m) => 
             MInterpreterWithCtx c m (ChoiceInstructions a) where
  interpretStepM _ (Choose alist nF) = do
        liftIO $ putStrLn $ "Make a pick " ++ show(alist)
        choiceStr <- liftIO $ getLine
        let choiceA = read choiceStr
        guard (elem choiceA alist)
        nF choiceA
\end{code}

We are all set for polyglot consumption of our DSL and interpreter!  
This will happen in my next example.  To finish and try out what I have done so far simply evaluate
runTestChoice' in GHCi. 
(interpretInM is analogous to interpretInIO we have seen above, see DslSupport module for its definition):
\begin{code}
whateverContext :: c
whateverContext = undefined

runTestChoice' ::  IO [Int]
runTestChoice' = interpretInM whateverContext (testChoiceProg 3 [0..10]) 
\end{code}
