import Data.Array
import Data.List
import Data.Char
import Data.Ix

type Position     = (Int, Int)
type Velocity     = (Int, Int)
type Target       = ((Int, Int), (Int, Int))

isInt :: Char -> Bool
isInt = (||) <$> isDigit <*> (== '-')

parseInput :: String -> Target
parseInput str = let
    beforeX = dropWhile (not . isInt) str
    x1      = takeWhile isInt beforeX
    x2      = takeWhile isInt $ drop (length x1 + 2) beforeX
    beforeY = dropWhile (not . isInt) $ drop (length x1 + length x2 + 2) beforeX
    y1      = takeWhile isInt beforeY
    y2      = takeWhile isInt $ drop (length y1 + 2) beforeY
    in ((read x1, read y1), (read x2, read y2))

boundsHeight :: Target -> Int
boundsHeight ((_, y1), (_, y2)) = abs (y1 - y2)

xMax, yMin :: Target -> Int
xMax ((x1, _), (x2, _)) = max x1 x2
yMin ((_, y1), (_, y2)) = min y1 y2

stepVel :: Velocity -> (Int, Int)
stepVel (x, y) = (max (x - 1) 0, y - 1)

computeTrajectory :: Int -> Int -> Velocity -> [Position]
computeTrajectory xMax yMin = 
    takeWhile (\(x, y) -> x <= xMax && y >= yMin) . map (foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)) . inits . iterate stepVel


{-
Process: We can find the maximum possible Y-directional velocity.
         Wait, an appropriate x-value always exists doesn't it?
-}

solver :: Int -> Target -> Int
solver maxYVel tgt = let
    testTraj = computeTrajectory (xMax tgt) (yMin tgt) 
    in undefined

main :: IO ()
main = do
    tgtIn <- parseInput <$> readFile "input"
    putStr "Solution 1: " >> print (boundsHeight tgtIn)