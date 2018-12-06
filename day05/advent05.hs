import Data.Char

isDestroyed :: Char -> Char -> Bool
isDestroyed x1 x2 | (toLower x1) /= (toLower x2) = False
                  | (isUpper x1) && (isLower x2) = True
                  | (isLower x1) && (isUpper x2) = True
                  | otherwise = False

reducePolymer :: String -> String -> String
reducePolymer [] xs = xs
reducePolymer [x] xs = xs ++ [x]
reducePolymer (x1:x2:xs) [] | isDestroyed x1 x2 = reducePolymer xs []
                            | otherwise = reducePolymer (x2:xs) [x1]
reducePolymer (x1:x2:xs) ys | isDestroyed x1 x2 = reducePolymer ([(last ys)] ++ xs) (init ys)
                            | otherwise = reducePolymer (x2:xs) (ys ++ [x1])

main :: IO()
main = do 
    input <- readFile "data05.txt"
    let reducedPoly = reducePolymer input []
    putStrLn $ show $ length reducedPoly