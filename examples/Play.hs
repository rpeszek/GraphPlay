
import qualified Play.DGraph.FoldsOnSimpleGraph as FSG
import qualified Play.DGraph.MonoidFoldsOnSimpleGraph as MFSG
import qualified Play.DGraph.IndexedFolds as IND1


main :: IO ()
main = do
  print "Folds:"
  print FSG.experiments
  print "\nMonoid Folds:"
  print MFSG.experiments
  print "\nIndexed Folds:"
  print IND1.experiments
