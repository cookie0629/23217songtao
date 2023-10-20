-- num 1
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

-- num 2
evenOnly :: [Int] -> [Int]
evenOnly = foldl (\acc x -> if even (length acc + 1) then acc ++ [x] else acc) []

-- num 3
for :: (Int, a) -> (Int -> Int) -> (Int -> Bool) -> (Int -> a -> a) -> a
for (counter, accumulator) incrementPredicate nextPredicate body =
  if nextPredicate counter
    then for (increment counter, body counter accumulator) incrementPredicate nextPredicate body
    else accumulator
  where
    increment x = x + 1

-- num 4
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = concatMap (\x -> map (\y -> (x, y)) ys) xs

-- num 5
type BinaryRelation a = Eq a => [(a, a)]
refl :: Eq a => BinaryRelation a -> Bool
refl rel = all (\(x, y) -> (y, x) `elem` rel) rel

