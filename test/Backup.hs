import Test.Hspec
import Test.QuickCheck
import Deprecated.DiGraph.SampleData

main :: IO ()
main = hspec $ do
  describe "env sanity test" $ do
    context "read" $ do
      it "is inverse to show" $ property $
        \x -> (read . show) x == (x :: Int)
  describe "can access src" $ do
      it "accessing sample data" $ do
        length testEdges == 10
