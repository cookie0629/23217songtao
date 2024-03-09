-- num 1
import Control.Monad.State

fact' :: State (Int, Int) Int
fact' = do
    (step, accumulator) <- get
    if step == 0
        then return accumulator
        else do
            put (step-1, step*accumulator)
            fact'

fact :: Int -> Int
fact n = evalState fact' (n, 1)

-- num 2
fibb' :: State (Int, Int, Int) Int
fibb' = do
  (step, n1, n2) <- get
  if step == 0
    then return n2
    else do
      put (step - 1, n1 + n2, n1)
      fibb'

fibb :: Int -> Int
fibb n = evalState fibb' (n, 1, 0)

-- num 3
data BinTree a =
  Nil |
  Node (BinTree a) a (BinTree a)
  deriving Show

numberTree :: BinTree () -> BinTree Integer
numberTree tree = evalState (numberTree' tree) 0
  where
    numberTree' :: BinTree () -> State Integer (BinTree Integer)
    numberTree' Nil = return Nil
    numberTree' (Node left _ right) = do
      numberedLeft <- numberTree' left
      currentNumber <- get
      put (currentNumber + 1)
      numberedRight <- numberTree' right
      return (Node numberedLeft currentNumber numberedRight)