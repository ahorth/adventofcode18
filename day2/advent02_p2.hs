import Data.List

isIds :: Bool -> String -> String -> Bool
isIds isDifferent [] [] = isDifferent
isIds False (x:xs) (y:ys) | x /= y = isIds True xs ys
                          | otherwise = isIds False xs ys
isIds True (x:xs) (y:ys) | x /= y = isIds False [] []
                          | otherwise = isIds True xs ys

-- from https://stackoverflow.com/questions/34044366/how-to-extract-all-unique-pairs-of-a-list-in-haskell
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

concatString :: String -> String -> String -> String
concatString [] [] zs =zs
concatString (x:xs) (y:ys) zs | x == y = concatString xs ys (zs ++ [x])
                              | otherwise = concatString xs ys zs

solve :: [(String, String)] -> String
solve ((x,y):xs) | isIds False x y = concatString x y ""
                 | otherwise = solve xs
                 
main :: IO()
main = do
    content <- readFile "data02.txt"
    let parsed = lines content
    let ps = pairs parsed
    let key = solve ps 
    putStrLn $ show key