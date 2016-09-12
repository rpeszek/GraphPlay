-- WORK in progress

module S2_Free.E07_ChoiceDSL where

import Control.Monad (MonadPlus, guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Free (Free(..), liftF)
import PolyGraph.Common.DslSupport (MInterpreterWithCtx (..), execInM)
import PolyGraph.Common.DslSupport.Coproduct ((:<:), liftDSL)

data ChoiceInstructions a r =  Choose [a] (a -> r)
                                  deriving Functor
                  
type ChoiceDSL a r = Free (ChoiceInstructions a) r


choose ::  forall a polyglot. (Functor polyglot, (ChoiceInstructions a) :<: polyglot) 
                         => [a] -> Free polyglot a
choose list =  liftDSL $ liftF (Choose list id)

interpretChoices :: (Show a, Read a, Eq a) => ChoiceDSL a r -> IO r
interpretChoices (Free (Choose alist nF)) = do
      putStrLn $ "Make a pick " ++ show(alist)
      choiceStr <- getLine
      let choiceA = read choiceStr
      guard (elem choiceA alist)
      interpretChoices $ nF choiceA
interpretChoices (Pure r) = return r

testChoice :: Int -> [a] -> ChoiceDSL a [a]
testChoice noPicks alist = do
   mapM (const $ choose alist) [1 .. noPicks]
   
runTestChoice :: IO [Int]
runTestChoice = interpretChoices $ testChoice 3 [0..10]

instance (Show a, Read a, Eq a, MonadIO m, MonadPlus m) => MInterpreterWithCtx c m (ChoiceInstructions a) where
  interpretM _ (Choose alist nF) = do
        liftIO $ putStrLn $ "Make a pick " ++ show(alist)
        choiceStr <- liftIO $ getLine
        let choiceA = read choiceStr
        guard (elem choiceA alist)
        nF choiceA

whateverContext :: c
whateverContext = undefined

runTestChoice' ::  IO [Int]
runTestChoice' = execInM whateverContext (testChoice 3 [0..10]) 
