
import qualified Deprecated.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Deprecated.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Deprecated.DiGraph.IndexedFolds as IND1
import qualified Build.E01_UnsafeDiamond as E01
import qualified Build.E02_SafeDiamond as E02
import qualified Deprecated.DiGraph.PolyRebuild as PRB

main :: IO ()
main = do
  putStrLn "Folds:"
  print FSG.experiments
  putStrLn "Monoid Folds:"
  print MFSG.experiments
  putStrLn "Indexed Folds:"
  print IND1.experiments
  putStrLn "E01:"
  E01.allThisHardWork
  putStrLn "E02:"
  putStrLn E02.showDiamond'0123_1
  putStrLn "PolyRebuild"
  putStrLn PRB.showDiamond3456AsHashMap
  putStrLn PRB.showDiamondChainAsHashMap
