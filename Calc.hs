module Calc where
  import Expr

  evalExpr :: Expr -> Int
  evalExpr (Num i) = i
  evalExpr (Add exp1 exp2) = (evalExpr exp1) + (evalExpr exp2)
  evalExpr (Multiply exp1 exp2) = (evalExpr exp1) * (evalExpr exp2)
