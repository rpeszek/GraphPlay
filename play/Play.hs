
import qualified Deprecated.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Deprecated.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Deprecated.DiGraph.IndexedFolds as IND1
import qualified Build.E01_UnsafeDiamond as ADJ
import qualified Build.E02_SafeDiamond as ADJ
import qualified Deprecated.DiGraph.PolyRebuild as PRB

main :: IO ()
main = do
  putStrLn "Folds:"
  print FSG.experiments
  putStrLn "Monoid Folds:"
  print MFSG.experiments
  putStrLn "Indexed Folds:"
  print IND1.experiments
  putStrLn "Adj 1:"
  putStrLn ADJ.showDiamond0123_1
  putStrLn ADJ.showDiamond0123_2
  putStrLn ADJ.showDiamond0123_3
  putStrLn ADJ.showDiamond0123_4
  putStrLn ADJ.showDiamond0123_5
  putStrLn ADJ.showDiamond0123_6
  putStrLn ADJ.showDiamond0123_7
  putStrLn ADJ.showDiamond'0123_1
  putStrLn "PolyRebuild"
  putStrLn PRB.showDiamond3456AsHashMap
  putStrLn PRB.showDiamondChainAsHashMap
