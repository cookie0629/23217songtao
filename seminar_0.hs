-- num 1
double_1 :: [a] -> [a]
double_1 [] = []
double_1 (x:xs) = x : x : double_1 xs

-- double_2 = (\xs -> map (\x -> [x, x]) xs)
double_2 :: [a] -> [a]
double_2 xs = xs >>= (\x -> [x, x])

-- num 2
data PeanoNatural = Zero | Succ PeanoNatural
-- 转换为整数
natToInt :: PeanoNatural -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n
