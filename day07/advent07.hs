import qualified Data.List as List
import qualified Data.Set as Set


getStep :: [(Char, Char)] -> [Char]
getStep [] = []
getStep xs = Set.toList $ Set.fromList ([x | x <- (map fst xs)] ++ [x | x <- (map snd xs)])

getParents :: [Char] -> [(Char, Char)] -> [(Char, [Char])] -> [(Char, [Char])]
getParents [] _ parents = parents
getParents (x:xs) ys ps = let parents = [fst z | z<- ys, (snd z) == x] in
                           getParents xs ys ((x, parents):ps)

updateParents :: [(Char, [Char])] -> Char -> [(Char, [Char])]
updateParents ps x = [(fst p, [y | y <- (snd p), y /=x ]) | p <- ps, (fst p) /= x]


findOrder :: [(Char, [Char])] -> [Char] -> [Char]
-- findOrder _ [] = []
findOrder [] ordered = ordered
findOrder ps ordered = findOrder (f next_letter) (ordered ++ [next_letter]) where
                        candidates = List.sort [fst c | c <- ps, (length $ snd c) == 0] 
                        next_letter = head candidates
                        f = updateParents ps


parseInput :: String -> [(Char, Char)]
parseInput raw = [(line !! 5, line !! 36) | line <- lines raw]

main :: IO ()
main = do 
    raw <- readFile "data07.txt"
    let steps = parseInput raw
    putStrLn (findOrder (getParents (getStep steps) steps []) [])
-- steps = [('C','A'), ('C','F'), ('A','B'), ('A','D'), ('B','E'), ('D','E'), ('F','E')]