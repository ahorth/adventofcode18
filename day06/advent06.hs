import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord, Read)

manhathanDistance :: Point -> Point -> Int
manhathanDistance (Point x1 y1) (Point x2 y2) = abs (x2 - x1) + abs (y2 - y1)

manhathanDistances :: [Point] -> Point -> [Int]
manhathanDistances pts pt = foldr (\x y -> (manhathanDistance pt x):y) [] pts

closestPoint :: [Point] -> Point -> Maybe Point
closestPoint pts pt | snd (dists !! 0) == snd (dists !! 1) = Nothing
                    | otherwise  = Just $ fst (head dists) 
                      where dists = sortOn snd $ zip pts (manhathanDistances pts pt)

edgePoints :: [Point] -> (Point, Point )
edgePoints pts = ((Point x1 y1), (Point x2 y2)) where
                    x1 = minimum [x | (Point x y) <- pts]
                    y1 = minimum [y | (Point x y) <- pts]
                    x2 = maximum [x | (Point x y) <- pts]
                    y2 = maximum [y | (Point x y) <- pts]

perimeterPoints :: Point -> Point -> [Point]
perimeterPoints (Point x1 y1) (Point x2 y2) = t ++ b++ l ++ r where
                                                        t = [Point x y1 | x<- [x1..x2]]
                                                        b = [Point x y2 | x<- [x1..x2]]
                                                        l = [Point x1 y | y<- [(y1+1)..(y2-1)]]
                                                        r = [Point x2 y | y<- [(y1+1)..(y2-1)]]

gridPoints :: Point -> Point -> [Point]
gridPoints (Point x1 y1) (Point x2 y2) = [Point x y | x <- [x1..x2], y <- [y1..y2]]

largestEnclosed :: [Maybe Point] -> [Maybe Point] -> Int
largestEnclosed grid perimeter = let notPerim = [pt | pt <- grid,
                                                      not $ pt `elem` perimeter,
                                                      pt /= Nothing] in 
                                  maximum $ map length (group $ sort notPerim)


parsePoint :: String -> Point
parsePoint ptStr = read $ "Point " ++ [c | c<-ptStr, c /= ',']

distanceTotal pts pt = sum $ manhathanDistances pts pt

largestSmaller :: [Point] -> [Point] -> Int
largestSmaller grid anchors = let total_dist = distanceTotal anchors in 
                              length $ filter (<10000) $ map total_dist grid

main :: IO()
main = do
    parsePts <- readFile "data06.txt"
    let pts = sort $ map parsePoint (lines parsePts)

    -- let pts = [Point 1 1, Point 1 6, Point 8 3, Point 3 4, Point 5 5, Point 8 9]
    let (topLeftPt, botRightPt)  = edgePoints pts
    putStrLn $ show botRightPt
    -- let botRightPt = last $ sort pts
    let perim = perimeterPoints topLeftPt botRightPt
    -- putStrLn $ show perim
    let grid  = gridPoints topLeftPt botRightPt
    let closestPointsperim = map (closestPoint pts) perim
    let closestPointsgrid = map (closestPoint pts) grid
    let solution = largestEnclosed  closestPointsgrid closestPointsperim
    let solutionb = largestSmaller grid pts
    putStrLn $ show solution
    putStrLn $ show solutionb
