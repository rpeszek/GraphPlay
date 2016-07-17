
import qualified Play.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Play.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Play.DiGraph.IndexedFolds as IND1
import qualified Play.DiGraph.PolyBuild as ADJ
import qualified Play.DiGraph.PolyRebuild as PRB

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
  putStrLn ADJ.showDiamond'0123_1
  putStrLn "PolyRebuild"
  putStrLn PRB.showDiamond3456AsHashMap
  putStrLn PRB.showDiamondChainAsHashMap
