{--
 Growable square matrix
--}

module PolyGraph.Common.BuildableMatrix (
   MatrixDataSet
   , emptyMatrix
   , addMatrixElement
   , toDataMatrix
) where

import qualified Data.Foldable as F
import qualified Data.Sequence as S
import qualified Data.Matrix as M


data MatrixDataSet a = MatrixDataSet {
    size:: Int,

    -- seq of rows
    elements :: S.Seq (S.Seq a)
}

emptyMatrix :: forall a . MatrixDataSet a
emptyMatrix = MatrixDataSet {size = 0, elements=S.empty }

growMatrix :: forall a . (Num a) => Int -> MatrixDataSet a -> MatrixDataSet a
growMatrix amount matrix = let newsize = (size matrix) + amount
                               appendToEachRow = S.replicate amount 0
                               appendRow  = S.replicate newsize 0                          :: Num a => S.Seq a
                               appendRows = S.replicate amount appendToEachRow             :: S.Seq (S.Seq a)
                               wider = fmap (\r -> r S.>< appendToEachRow) (elements matrix) :: S.Seq (S.Seq a)
                               square =  wider S.>< appendRows
                           in MatrixDataSet {size= newsize, elements = square}

-- auto-grows the matrix
-- | row -> column -> matrix
addMatrixElement :: forall a . (Num a) =>  Int -> Int -> a -> MatrixDataSet a -> MatrixDataSet a
addMatrixElement row column value matrix =
                  let newSize = (max row) . (max column) $ size matrix
                      growBy = newSize - row
                      newmatrix = if growBy > 0
                                  then growMatrix growBy matrix
                                  else matrix
                      oldelements = elements newmatrix
                      newelements = S.adjust (S.update column value) row
                  in undefined

toLists :: MatrixDataSet a -> [[a]]
toLists matrix = map (F.toList) (F.toList (elements matrix))

--TODO finish these

fromLists :: Int -> [[a]] -> MatrixDataSet a
fromLists size lists = undefined

toDataMatrix :: MatrixDataSet a -> M.Matrix a
toDataMatrix matrix = M.fromLists . toLists $ matrix

fromDataMatrix :: Int -> M.Matrix a -> MatrixDataSet a
fromDataMatrix size m = undefined
