
-- num 1
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- num 2
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs)
    | x `elem` xs = nub' xs
    | otherwise = x : nub' xs

union :: (Eq a) => [a] -> [a] -> [a]
union [] [] = []
union xs [] = xs
union [] ys = ys
union xs ys = nub' (xs ++ ys)

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection [] [] = []
intersection xs [] = []
intersection [] ys = []
intersection (x:xs) ys
    | x `elem` ys = x : intersection xs ys
    | otherwise = intersection xs ys

-- num 3
computeVector :: [String] -> [Double] -> [Double]
computeVector [] vector = vector
computeVector (instruction:instructions) vector =
    case instruction of
        "inc" -> computeVector instructions (map (+1) vector)
        "dec" -> computeVector instructions (map (subtract 1) vector)
        "double" -> computeVector instructions (map (*2) vector)
        _ -> computeVector instructions vector

-- num 4
isInstruction :: String -> Bool
isInstruction str = str `elem` ["inc", "dec", "double", "sqrt", "halveIfPositive"]

cleaner :: [String] -> [String]
cleaner = filter isInstruction

-- num 5
optimizer :: [String] -> [String]
optimizer [] = []
optimizer (x:[]) = [x]
optimizer (x:y:zs)
    | (x == "_" && y == x) || (x == "dec" && y == "inc") || (x == "inc" && y == "dec") = optimizer zs
    | otherwise = x : optimizer (y:zs)


-- num 6
aboveGraph :: [(Double, Double)] -> (Double -> Double) -> [(Double, Double)]
aboveGraph points f = filter (\(x, y) -> y > f x) points 