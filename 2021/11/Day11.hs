import Data.List.Split
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Control.Monad
import Control.Monad.Loops
import Data.Char 
import System.IO
import Data.Functor

type Coordinate = (Int, Int)
type SquidGrid = IOArray Coordinate Int

neighbors :: Coordinate -> [Coordinate]
neighbors (x,y) = filter (inRange ( (0, 0), (9, 9) )) [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1)]

addOne :: SquidGrid -> Coordinate -> IO ()
addOne grid coord = freeze grid >>= writeArray grid coord . (1 +) . (! coord)

checkBurst :: SquidGrid -> Coordinate -> IO [Coordinate]
checkBurst grid coord = do
        val <- readArray grid coord
        if val > 9 then burst grid coord else return []

burst :: SquidGrid -> Coordinate -> IO [Coordinate]
burst grid coord = (writeArray grid coord 0 >> forM_ (neighbors coord) (addOne grid) >> forM (neighbors coord) (checkBurst grid)) <&> (coord :) . concat

step :: [Coordinate] -> SquidGrid -> IO Int
step coords grid = forM_ coords (addOne grid) >> length . concat <$> forM coords (checkBurst grid)

showGridElem :: Int -> String
showGridElem 0 = "\x1B[31m0\x1B[0m"
showGridElem x = show x

showSquidGrid :: SquidGrid -> IO ()
showSquidGrid grid = freeze grid >>= putStrLn . unlines . map (unwords . map showGridElem) . chunksOf 10 . elems

gridSync :: SquidGrid -> IO Bool
gridSync grid = freeze grid <&> all (== 0) . elems

runSim1For :: Int -> IO ()
runSim1For x = do
    input <- map (map digitToInt) . lines <$> readFile "input"
    let columns = length $ head input
    let rows    = length input
    let staticArray = listArray ((0, 0), (columns - 1, rows - 1)) (concat input)
    grid <- thaw staticArray
    burstsAtSteps <- replicateM x (step (indices staticArray) grid)
    showSquidGrid grid
    putStr "Solution 1: " >> print (sum burstsAtSteps)

runSim2 :: IO ()
runSim2 = do
    input <- map (map digitToInt) . lines <$> readFile "input"
    let columns = length $ head input
    let rows    = length input
    let staticArray = listArray ((0, 0), (columns - 1, rows - 1)) (concat input)
    grid <- thaw staticArray :: IO SquidGrid
    numberOfRuns <- length <$> untilM (step (indices staticArray) grid) (gridSync grid)
    showSquidGrid grid
    putStr "Solution 2: " >> print numberOfRuns

main :: IO ()
main = runSim1For 100 >> runSim2
