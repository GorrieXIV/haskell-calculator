module Expr where

  data Expr = Num Int | Add Expr Expr | Multiply Expr Expr

  -- we can create expressions the following way:
  -- 10 + 4 can be written as 'Func Add (I 10) (I 4)
  e1 :: Expr 
  e1 = Add (Num 123) (Num 456)

  parens s = '(' : s ++ ")"

  showExpr :: Expr -> String
  showExpr (Num k)          = show k
  showExpr (Multiply e1 e2) = parens (showExpr e1 ++ ("*" ++ showExpr e2))
  showExpr (Add e1 e2)      = parens (showExpr e1 ++ ("+" ++ showExpr e2))
