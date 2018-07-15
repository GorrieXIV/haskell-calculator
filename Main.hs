import Calc
import Expr

parseToExpr :: [String] -> Expr
parseToExpr [] = Num 0
parseToExpr ("(":"multiply":xs) = Multiply (parseToExpr (findArg1 xs [] 0)) (parseToExpr (findArg2 xs [] 0))
parseToExpr ("(":"add":xs) = Add (parseToExpr (findArg1 xs [] 0)) (parseToExpr (findArg2 xs [] 0))
parseToExpr (p:[]) = Num (read p :: Int)

findArg1 :: [String] -> [String] -> Int -> [String]
findArg1 [] ys n = ys
findArg1 ("(":xs) ys n = (findArg1 xs (ys ++ "(":[]) (n+1))
findArg1 (")":xs) ys 1 = (ys ++ ")":[])
findArg1 (")":xs) ys n = (findArg1 xs (ys ++ ")":[]) (n-1))
findArg1 (x:xs) [] 0 = (x:[])
findArg1 (x:xs) ys n = (findArg1 xs (ys ++ x:[]) n)

findArg2 :: [String] -> [String] -> Int -> [String]
findArg2 (x:")":[]) [] n = x:[]
findArg2 (")":xs) ys  (-1) = (init xs)
findArg2 ("(":xs) ys n = findArg2 xs ys (n+1)
findArg2 (")":xs) ys n = findArg2 xs ys (n-1)
findArg2 (x:xs) [] 0 = (init xs)
findArg2 (x:xs) [] n = findArg2 xs n

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
          print $ (showExpr (parseToExpr (separateBraces (words x))))
