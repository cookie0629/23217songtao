-- num1
data Person = Person {
    name :: String,
    surname :: String,
    father :: Maybe Person,
    mother :: Maybe Person
} deriving Show

--用模式匹配方法,不使用do表达式,找到外祖父 
mothersFather :: Person -> Maybe Person
mothersFather p = case mother p of
    Nothing -> Nothing
    Just m -> case father m of
        Nothing -> Nothing
        Just f -> Just f

-- 一元样式,可以使用do表达式
mothersFather' :: Person -> Maybe Person
mothersFather' p = do
    m <- mother p
    f <- father m
    return f

-- num2
hasGrandParents :: Person -> Maybe Person
hasGrandParents p = do
    m <- mother p
    f <- father p
    gm <- mother m
    gf <- father m
    gmf <- father f
    return gmf

-- num3
sumTwoInts :: IO ()
sumTwoInts = readLn >>= \x ->
              readLn >>= \y ->
              let result = x + y
              in print result






