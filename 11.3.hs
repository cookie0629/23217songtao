-- num 1
newtype Stack = Stack [Int]

emptyStack :: Stack
emptyStack = Stack []

push :: Stack -> Int -> Stack
push (Stack xs) x = Stack (x:xs)

pop :: Stack -> (Int, Stack)
pop (Stack []) = error "Empty stack"
pop (Stack (x:xs)) = (x, Stack xs)

-- num 2
data Instruction = Push Int | Add | Sub | Div | Mul | Pow deriving Show

computeInstructions :: [Instruction] -> Int
computeInstructions instructions = go instructions []
  where
    go [] [result] = result
    go (Push x:is) stack = go is (x:stack)
    go (Add:is) (v1:v2:stack) = go is (v1 + v2 : stack)
    go (Sub:is) (v1:v2:stack) = go is (v2 - v1 : stack)
    go (Div:is) (v1:v2:stack) = go is (v2 `div` v1 : stack)
    go (Mul:is) (v1:v2:stack) = go is (v1 * v2 : stack)
    go (Pow:is) (v1:v2:stack) = go is (v2 ^ v1 : stack)
    go _ _ = error "Invalid"


