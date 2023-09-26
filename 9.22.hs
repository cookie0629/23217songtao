
f :: Double -> Double
f x = x * 3
g :: Double -> Double
g x = x * 4
p :: Double
p = 2.44
maxFunction :: (Double -> Double) -> (Double -> Double) -> Double -> Double
maxFunction f g p = max (f p) (g p)


maxExpFunction :: (Double -> Double) -> Double -> Double
maxExpFunction f p = maxFunction exp f p


f1 :: Double -> Double
f1 x = x * 3
n :: Int
n = 4
applyFunctionN :: (Double -> Double) -> Double -> Int -> Double
applyFunctionN f p n
  | n < 0 = error "No!!!!!!"
  | n == 0 = p
  | otherwise = applyFunctionN f1 (f1 p) (n-1)

