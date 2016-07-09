
import qualified Play.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Play.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Play.DiGraph.IndexedFolds as IND1
import qualified Play.DiGraph.Adjust as ADJ

main :: IO ()
main = do
  print "Folds:"
  print FSG.experiments
  print "Monoid Folds:"
  print MFSG.experiments
  print "Indexed Folds:"
  print IND1.experiments
  print "Adj 1:"
  print ADJ.displayABCD1
