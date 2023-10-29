-- num 1
type Matrix = [((Int, Int), Double)]

makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix size elems = fillMatrix (replicate (fst size) (replicate (snd size) 0)) elems
  where fillMatrix matrix [] = concat matrix
        fillMatrix matrix (((i, j), elem):rest) = fillMatrix (replaceElem matrix i j elem) rest
        replaceElem matrix i j elem = take i matrix ++ [replaceRow (matrix !! i) j elem] ++ drop (i+1) matrix
        replaceRow row j elem = take j row ++ [elem] ++ drop (j+1) row

(!!!) :: Matrix -> (Int, Int) -> Double
m !!! (i, j) = case lookup (i, j) m of
  Just elem -> elem
  Nothing -> error "Element not found in matrix"

matrixSize :: Matrix -> (Int, Int)
matrixSize m = (maximum (map (fst . fst) m) + 1, maximum (map (snd . fst) m) + 1)

matrixIndices :: Matrix -> [(Int, Int)]
matrixIndices m = map fst m

-- num 2
matrixFold :: (b -> ((Int, Int), Double) -> b) -> b -> Matrix -> b
matrixFold f acc [] = acc
matrixFold f acc (x:xs) = matrixFold f (f acc x) xs

matrixMap :: (((Int, Int), Double) -> Double) -> Matrix -> Matrix
matrixMap f = map (\(i, e) -> (i, f (i, e)))

-- num 3
instance Show Matrix where
  show (Matrix arr) = concat [concat [show (arr ! (i, j)) ++ " " | j <- [0..cols]] ++ "\n" | i <- [0..rows]]
    where
      ((0, 0), (rows, cols)) = bounds arr

-- num 4
transpose :: Matrix -> Matrix
transpose mtx = map swapIndex mtx
  where swapIndex ((i, j), elem) = ((j, i), elem)

-- num 5
instance Eq Matrix where
  m1 == m2 = matrixSize m1 == matrixSize m2 && allElementsEqual m1 m2
    where allElementsEqual [] [] = True
          allElementsEqual [] _ = False
          allElementsEqual _ [] = False
          allElementsEqual (x:xs) (y:ys) = x == y && allElementsEqual xs ys

-- num 6



-- num 7



-- num 8


isSymmetrical :: Matrix -> Bool
isSymmetrical matrix = matrix == transpose matrix



