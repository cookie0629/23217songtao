-- num 1 
import Data.Array
newtype Matrix = Matrix (Array (Int, Int) Double)

makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix size assocList = Matrix $ array ((1, 1), size) assocList

(!!!) :: Matrix -> (Int, Int) -> Double
(Matrix arr) !!! idx = arr ! idx

matrixSize :: Matrix -> (Int, Int)
matrixSize (Matrix arr) = let ((r1, c1), (r2, c2)) = bounds arr in (r2, c2)

matrixIndices :: Matrix -> [(Int, Int)]
matrixIndices (Matrix arr) = indices arr

-- num 2
type MtxElem = ((Int, Int), Double)

matrixFold :: (b -> MtxElem -> b) -> b -> [[Double]] -> b
matrixFold f acc mtx = foldl (\acc' row -> foldl f acc' (zipWith (\j e -> ((j, length row), e)) [1..] row)) acc mtx

matrixMap :: (MtxElem -> Double) -> [[Double]] -> [[Double]]
matrixMap f mtx = map (map (\((_, _), e) -> f ((0, 0), e))) (zipWith (\i row -> zipWith (\j e -> ((i, j), e)) [1..] row) [1..] mtx)

matrixSumElems :: [[Double]] -> Double
matrixSumElems mtx = matrixFold (\acc (_, e) -> acc + e) 0 mtx

matrixMulScalar :: Double -> [[Double]] -> [[Double]]
matrixMulScalar x mtx = matrixMap (\(_, e) -> x * e) mtx

-- num 3
instance Show Matrix where
  show (Matrix arr) = concat [concat [show (arr ! (i, j)) ++ " " | j <- [0..cols]] ++ "\n" | i <- [0..rows]]
    where
      ((0, 0), (rows, cols)) = bounds arr

-- num 4
transpose :: Matrix -> Matrix
transpose (Matrix arr) =
  let ((iMin, jMin), (iMax, jMax)) = bounds arr
      transposedArr = array ((jMin, iMin), (jMax, iMax))
                    [((j, i), arr ! (i, j)) | i <- [iMin..iMax], j <- [jMin..jMax]]
  in Matrix transposedArr

-- num 5
instance Eq Matrix where
  (Matrix arr1) == (Matrix arr2) =
    let bounds1@((iMin1, jMin1), (iMax1, jMax1)) = bounds arr1
        bounds2@((iMin2, jMin2), (iMax2, jMax2)) = bounds arr2
    in bounds1 == bounds2 && and [arr1 ! (i, j) == arr2 ! (i, j) | i <- [iMin1..iMax1], j <- [jMin1..jMax1]]

-- num 6


-- num 7
isMatrix2x2 :: Matrix -> Bool
isMatrix2x2 (Matrix arr) = let ((iMin, jMin), (iMax, jMax)) = bounds arr in iMax == 1 && jMax == 1

isMatrix3x3 :: Matrix -> Bool
isMatrix3x3 (Matrix arr) = let ((iMin, jMin), (iMax, jMax)) = bounds arr in iMax == 2 && jMax == 2

-- num 8
isDiagonal :: Matrix -> Bool
isDiagonal (Matrix arr) =
  let ((iMin, jMin), (iMax, jMax)) = bounds arr
  in iMin == jMin && iMax == jMax && all (\(i, j) -> i == j || arr ! (i, j) == 0.0) (range ((iMin, jMin), (iMax, jMax)))

isSymmetrical :: Matrix -> Bool
isSymmetrical (Matrix arr) =
  let ((iMin, jMin), (iMax, jMax)) = bounds arr
  in iMin == jMin && iMax == jMax && all (\(i, j) -> arr ! (i, j) == arr ! (j, i)) (range ((iMin, jMin), (iMax, jMax)))


