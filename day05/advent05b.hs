import Data.Char
import Data.List 
isDestroyed :: Char -> Char -> Bool
isDestroyed x1 x2 | (toLower x1) /= (toLower x2) = False
                  | (isUpper x1) && (isLower x2) = True
                  | (isLower x1) && (isUpper x2) = True
                  | otherwise = False

filterPolymer :: Char -> String -> String 
filterPolymer x = filter (\y -> toLower(y)/=x)

reducePolymer :: String -> String -> String
reducePolymer ys [] = ys
reducePolymer ys [x] = ys ++ [x]
reducePolymer [] (x1:x2:xs) | isDestroyed x1 x2 = reducePolymer [] xs
                            | otherwise = reducePolymer [x1] (x2:xs)
reducePolymer ys (x1:x2:xs) | isDestroyed x1 x2 = reducePolymer (init ys) ([(last ys)] ++ xs)
                            | otherwise = reducePolymer (ys ++ [x1]) (x2:xs) 


main :: IO()
main = do 
    input <- readFile "data05.txt"
    let smallestPoly = zipWith (\x y -> length $ reducePolymer [] (filterPolymer x y) ) ['a'..'z'] (replicate 24 input)
    putStrLn $ show $ minimum smallestPoly