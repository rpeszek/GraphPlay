
import qualified Play.DGraph.TreeFoldOnSimpleGraph as FSG
import qualified Play.DGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Play.DGraph.IndexedFolds as IND1


main :: IO ()
main = do
  print "Folds:"
  print FSG.experiments
  print "Monoid Folds:"
  print MFSG.experiments
  print "Indexed Folds:"
  print IND1.experiments
