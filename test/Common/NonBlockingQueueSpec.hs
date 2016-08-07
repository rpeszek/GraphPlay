
module Common.NonBlockingQueueSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified PolyGraph.Common.NonBlockingQueue.Properties as QProp

isFifo :: ([Int],[QProp.QueueInstruction]) -> Bool
isFifo = QProp.isFifo

dequeuesUnlessEmpty :: ([Int],[QProp.QueueInstruction]) -> Bool
dequeuesUnlessEmpty = QProp.dequeuesUnlessEmpty

spec :: Spec
spec = do
  describe "NonBlockingQueue" $ do
    it "isFifo" $ property $
          isFifo 
    it "dequeues with something unless queue is empty" $ property $
          dequeuesUnlessEmpty

main :: IO ()
main = hspec spec
