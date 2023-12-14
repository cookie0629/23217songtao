type VarName = String

data LambdaTerm
  = Lam VarName LambdaTerm
  | App LambdaTerm LambdaTerm
  | Var VarName

instance Show LambdaTerm where
  show :: LambdaTerm -> String
  show (Var x) = x
  show (Lam x t) = "(λ " ++ x ++ " . " ++ show t ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

exampleTerm1 :: LambdaTerm
exampleTerm1 = Lam "x" (Var "x")
exampleTerm2 :: LambdaTerm
exampleTerm2 = Lam "n" (App (Var "f") (Var "n"))
exampleTerm3 :: LambdaTerm
exampleTerm3 = Lam "f" (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))


-- num 2
substitute :: LambdaTerm -> VarName -> LambdaTerm -> LambdaTerm
substitute (Var y) x term
  | y == x = term
  | otherwise = Var y
substitute (Lam y t) x term
  | y == x = Lam y t
  | otherwise = Lam y (substitute t x term)
substitute (App t1 t2) x term = App (substitute t1 x term) (substitute t2 x term)

betaReduce :: LambdaTerm -> LambdaTerm
betaReduce (App (Lam x t1) t2) = substitute t1 x t2
betaReduce term = term

-- num 3
eval :: LambdaTerm -> LambdaTerm
eval = eval0 0
  where
    eval0 :: Int -> LambdaTerm -> LambdaTerm
    eval0 c term
      | canBeReduced term && c < maxCount = eval0 (c+1) (reduceSubterms term)
      | otherwise = term
    maxCount = 1000

canBeReduced :: LambdaTerm -> Bool
canBeReduced (App (Lam _ _) _) = True
canBeReduced (App t1 t2) = canBeReduced t1 || canBeReduced t2
canBeReduced (Lam _ t) = canBeReduced t
canBeReduced _ = False

reduceSubterms :: LambdaTerm -> LambdaTerm
reduceSubterms (App (Lam x t1) t2) = substitute t1 x t2
reduceSubterms (App t1 t2) = if canBeReduced t1 then App (reduceSubterms t1) t2 else App t1 (reduceSubterms t2)
reduceSubterms (Lam x t) = Lam x (reduceSubterms t)
reduceSubterms term = term

