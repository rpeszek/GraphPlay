
module Common.NonBlockingQueueSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import qualified PolyGraph.Common.NonBlockingQueue.Properties as QProp

spec :: Spec
spec = do
  describe "NonBlockingQueue" $ do
    it "isFifo" $ property $
          (QProp.isFifo :: [QProp.QueueInstruction Int] -> Bool)
    it "dequeues with something unless queue is empty" $ property $
          (QProp.dequeuesUnlessEmpty :: [QProp.QueueInstruction Int] -> Bool)

main :: IO ()
main = hspec spec
