even' :: Int -> Bool
even' n = n `mod` 2 == 0

odd' :: Int -> Bool
odd' n = not (even' n)

computeFibonacci :: Int -> Int
computeFibonacci n
    | n <= 0 = error "No"
    | n == 1 = 0
    | n == 2 = 1
    | otherwise = computeFibonacci (n-1) + computeFibonacci (n-2)

sumOddFibonacci :: Int -> Int
sumOddFibonacci n
    | n <= 0 = error "No"
    | otherwise = sum [x | x <- take n numbers, odd' x]
    where numbers = [computeFibonacci x | x <- [1..]]
