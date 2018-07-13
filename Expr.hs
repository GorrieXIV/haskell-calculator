module Expr where

  data Expr = Num Int | Add Expr Expr | Multiply Expr Expr

  -- we can create expressions the following way:
  -- 10 + 4 can be written as 'Func Add (I 10) (I 4)
  e1 :: Expr 
  e1 = Add (Num 123) (Num 456)
