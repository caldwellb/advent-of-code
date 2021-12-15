import Data.Array
import Data.Array.IO
import Data.Ix
import Data.Char
import Control.Monad
import GHC.IOArray (readIOArray)

type Coordinate = (Int, Int)
type Risk       = Int

parseInput :: String -> Array Coordinate Risk
parseInput str = let
    doubleList = map (map digitToInt) . lines $ str
    cols = length . head $ doubleList
    rows = length doubleList
    in listArray ((0,0), (cols - 1, rows - 1)) . concat $ doubleList

makeSolutionSpace :: Array Coordinate Risk -> IO (IOArray Coordinate Int)
makeSolutionSpace arr = do
    let bnds             = bounds arr
        neighbors (x, y) = filter (inRange bnds) [(x + 1, y), (x, y + 1)]
    solSpace <- newArray bnds 0
    forM_ (reverse $ indices arr) $ \(x, y) -> do
        case neighbors (x, y) of
            [] -> writeArray solSpace (x, y) 0
            xs -> do
                    lst <- mapM (readIOArray solSpace) xs
                    let riskInc = map (arr !) xs
                    writeArray solSpace (x, y) (minimum (zipWith (+) lst riskInc))
    return solSpace

incRisk :: Int -> Int
incRisk = (+ 1) . flip mod 9

incRiskBy :: Int -> Int -> Int
incRiskBy n x = iterate incRisk x !! n

parseInput2 :: String -> Array Coordinate Risk
parseInput2 str = let
        doubleList           = map (map digitToInt) . lines $ str
        horizExpand          = foldr1 (zipWith (++)) . map ((\f -> map (map f) doubleList) . incRiskBy) $ [0..4]
        vertExpand           = concatMap ((\f -> map (map f) horizExpand) . incRiskBy) [0..4]
        cols                 = length . head $ vertExpand
        rows                 = length vertExpand
    in listArray ((0,0), (cols - 1, rows - 1)) . concat $ vertExpand

main :: IO ()
main = do
    input     <- parseInput  <$> readFile "input"
    solSpace1 <- makeSolutionSpace input
    sol1      <- readIOArray solSpace1 (0,0)
    putStr "Solution 1: " >> print sol1
    input2    <- parseInput2 <$> readFile "input"
    solSpace2 <- makeSolutionSpace input2
    sol2      <- readIOArray solSpace2 (0,0)
    putStr "Solution 2: " >> print sol2
    return ()