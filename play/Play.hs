
import qualified Build.E01_UnsafeDiamond as E01
import qualified Build.E02_SafeDiamond as E02
import qualified Build.E03_PolyReloaded as E03
import qualified Build.E04_PolyToMax as E04
import qualified Build.E05_PolyForFree as E05
import qualified Deprecated.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Deprecated.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Deprecated.DiGraph.IndexedFolds as IND1
import qualified Deprecated.DiGraph.PolyRebuild as PRB

main :: IO ()
main = do
  putStrLn "E01:"
  E01.allThisHardWork
  putStrLn "E02:"
  E02.allThisHardWork
  putStrLn "E03:"
  E03.allThisHardWork
  putStrLn "E04:"
  E04.allThisHardWork
  putStrLn "E05:"
  E05.allThisHardWork
  putStrLn ""
  putStrLn "Examples that need rework:"
  putStrLn "Folds:"
  print FSG.experiments
  putStrLn "Monoid Folds:"
  print MFSG.experiments
  putStrLn "Indexed Folds:"
  print IND1.experiments
  putStrLn "PolyRebuild"
  putStrLn PRB.showDiamond3456AsHashMap
  putStrLn PRB.showDiamondChainAsHashMap
