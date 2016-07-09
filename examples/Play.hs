
import qualified Play.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Play.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Play.DiGraph.IndexedFolds as IND1
import qualified Play.DiGraph.Adjust as ADJ

main :: IO ()
main = do
  putStrLn "Folds:"
  print FSG.experiments
  putStrLn "Monoid Folds:"
  print MFSG.experiments
  putStrLn "Indexed Folds:"
  print IND1.experiments
  putStrLn "Adj 1:"
  putStrLn ADJ.showDiamondABCD1
  putStrLn ADJ.showDiamondABCD2
  putStrLn ADJ.showDiamondABCD3
