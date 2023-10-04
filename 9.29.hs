-- num.1
fibMod5 :: [Int]
fibMod5  = [x | x <- [computeFibonacci n | n <- [0,1..]],x `mod` 5 == 0]
computeFibonacci :: Int -> Int
computeFibonacci n
    | n <= 0 = error "No"
    | n == 1 = 0
    | n == 2 = 1
    | otherwise = computeFibonacci (n-1) + computeFibonacci (n-2)
-- -- take 7 fibMod5

-- num.2
perimeter :: [(Double, Double)] -> Double
perimeter [(x1, y1), (x2, y2), (x3, y3)] = 
    let a = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
        b = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
        c = sqrt ((x1 - x3)^2 + (y1 - y3)^2)
    in a + b + c

-- num.3
checkAllEq :: Eq a => [a] -> Bool
checkAllEq [] = True
checkAllEq [x] = True
checkAllEq (x:y:xs)
  | x /= y = False
  | otherwise = checkAllEq (y:xs)


-- num.4
type Point = (Double, Double)
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
minDistance :: [Point] -> Double
minDistance points = minimum [distance p1 p2 | p1 <- points, p2 <- points, p1 /= p2]


-- num.5
computeProgram :: [String] -> Double -> Double
computeProgram [] p = p
computeProgram (x:xs) p
    | x == "inc" = computeProgram xs (p + 1)
    | x == "dec" = computeProgram xs (p - 1)
    | x == "double" = computeProgram xs (p * 2)
    | x == "sqrt" = computeProgram xs (sqrt p)
    | x == "halveIfPositive" = computeProgram xs (if p > 0 then p / 2 else p)
    | otherwise = computeProgram xs p

