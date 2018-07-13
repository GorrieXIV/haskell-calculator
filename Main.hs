import Calc
import Expr

parseToExpr :: [String] -> Expr
parseToExpr [] = Num 0
parseToExpr ("(":"multiply":xs) = Multiply (findArg1 xs [] 0) (findArg2 xs 0)
parseToExpr ("(":"add":xs) = Add (findArg1 xs [] 0) (findArg2 xs 0)
parseToExpr (p:[]) = Num (read p)

findArg1 :: [String] -> [String] -> Int -> Expr
findArg1 [] ys n = parseToExpr ys
findArg1 ("(":xs) ys n = (findArg1 xs (ys ++ "(":[]) (n+1))
findArg1 (")":xs) ys 1 = parseToExpr (ys ++ ")":[])
findArg1 (")":xs) ys n = (findArg1 xs (ys ++ ")":[]) (n-1))
findArg1 (x:xs) [] 0 = parseToExpr (x:[])
findArg1 (x:xs) ys n = (findArg1 xs (ys ++ x:[]) n)

findArg2 :: [String] -> Int -> Expr
findArg2 ("(":xs) n = findArg2 xs (n+1)
findArg2 (x:xs) 0 = parseToExpr (init xs)
findArg2 (")":xs) (-1) = parseToExpr (init xs)
findArg2 (")":xs) n = findArg2 xs (n-1)
findArg2 _ _ = error "incorrect format"

separateBraces :: [String] -> [String]
separateBraces [] = []
separateBraces (x:xs)
                | (braceStart x) = "(":(tail x):[] ++ (separateBraces xs)
                | (braceEnd x) = (sepTailBraces x) ++ (separateBraces xs)
                | otherwise = x:[] ++ (separateBraces xs)

braceStart :: String -> Bool
braceStart x = if head x == '(' then True
               else False

braceEnd :: String -> Bool
braceEnd (x:xs) = if xs /= [] then (braceEnd xs)
                else if x == ')' then True
                else False
    
sepTailBraces :: String -> [String]
sepTailBraces x = if (head (reverse x)) == ')' then (sepTailBraces (init x)) ++ ")":[]
                  else x:[]

main :: IO ()
main = do x <- readLn
          print $ (evalExpr (parseToExpr (separateBraces (words x))))
