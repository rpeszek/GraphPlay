
import qualified S1_Cstr.E01_UnsafeDiamond as E01
import qualified S1_Cstr.E02_SafeDiamond as E02
import qualified S1_Cstr.E03_PolyReloaded as E03
import qualified S1_Cstr.E04_PolyToMax as E04
import qualified S2_Free.E01_PolyForFree as E2_01
import qualified S2_Free.E02_BFS as E2_02
import qualified S2_Free.E03_BFS2 as E2_03
import qualified S2_Free.E04_SpanTree as E2_04
import qualified S2_Free.E05_Ratings as E2_05
import qualified S2_Free.E06_WalkAndRate as E2_06

import qualified Deprecated.DiGraph.TreeFoldOnSimpleGraph as FSG
import qualified Deprecated.DiGraph.TreeMonoidFoldOnSimpleGraph as MFSG
import qualified Deprecated.DiGraph.IndexedFolds as IND1
import qualified Deprecated.DiGraph.PolyRebuild as PRB

main :: IO ()
main = do
  putStrLn "E1_01:"
  E01.allThisHardWork
  putStrLn "E1_02:"
  E02.allThisHardWork
  putStrLn "E1_03:"
  E03.allThisHardWork
  putStrLn "E1_04:"
  E04.allThisHardWork
  putStrLn "E2_01:"
  E2_01.allThisHardWork
  putStrLn "E2_02:"
  E2_02.allThisHardWork
  putStrLn "E2_03:"
  E2_03.allThisHardWork
  putStrLn "E2_04:"
  E2_04.allThisHardWork
  putStrLn "E2_05:"
  E2_05.allThisHardWork
  putStrLn "E2_06:"
  E2_06.allThisHardWork
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
