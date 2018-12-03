import Data.List


isOccurrence :: Int -> String -> Bool
isOccurrence count    =   checkGroup . countGroup . group . sort  where
                          countGroup = map (\x -> count == length x)
                          checkGroup xs = True `elem` xs

countOccurences :: Int -> [String] -> Int
countOccurences count ins = length $ filter (==True) $ map (isOccurrence count) ins

checkSum :: [String] -> Int
checkSum xs = (twos xs) * (threes xs) where 
              twos = countOccurences 2
              threes = countOccurences 3

main :: IO()
main = do
    content <- readFile "data02.txt"
    let parsed = lines content

    putStrLn $ show (checkSum parsed)