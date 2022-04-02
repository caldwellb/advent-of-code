import Data.Array
import Data.Array.IO
import Data.Ix
import Data.Char
import Data.List
import Control.Monad.Loops
import Data.Functor
import Data.IORef
import qualified Data.Set as S
import Data.Maybe
import Data.List.Split

type Coordinate = (Int, Int)
type Risk       = Int

parseInput :: String -> Array Coordinate Risk
parseInput str = let
    doubleList = map (map digitToInt) . lines $ str
    cols = length . head $ doubleList
    rows = length doubleList
    in listArray ((0,0), (rows - 1, cols - 1)) . concat $ doubleList

incRisk :: Int -> Int
incRisk = (+ 1) . flip mod 9

incRiskBy :: Int -> Int -> Int
incRiskBy n x = iterate incRisk x !! n

neighbors :: Coordinate -> Coordinate -> [Coordinate]
neighbors maxBnds (x, y) = filter (inRange ((0,0), maxBnds)) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

parseFullInput :: String -> Array Coordinate Risk
parseFullInput str = let
        doubleList           = map (map digitToInt) . lines $ str
        horizExpand          = foldr1 (zipWith (++)) . map ((\f -> map (map f) doubleList) . incRiskBy) $ [0..4]
        vertExpand           = concatMap ((\f -> map (map f) horizExpand) . incRiskBy) [0..4]
        cols                 = length . head $ vertExpand
        rows                 = length vertExpand
    in listArray ((0,0), (rows - 1, cols - 1)) . concat $ vertExpand

step :: S.Set (Risk, Coordinate) -> Array Coordinate Risk -> Risk
step paths riskarray = let
    (_, maxBound@(xMax, yMax))  = bounds riskarray
    ((rsk, crd), rest) = fromJust $ S.minView paths
    in if crd == maxBound then rsk
    else let
        stepOn = S.map (\nbrcrd -> (rsk + riskarray ! nbrcrd, nbrcrd)) (S.fromList $ neighbors maxBound crd)
        in step (S.union stepOn rest) riskarray

displayGrid :: Show e  => Array (Int, Int) e -> IO ()
displayGrid arr = let
    columns = snd . snd . bounds $ arr
    toPrint = chunksOf (columns + 1) . elems $ arr
    in putStr . unlines $ map (concatMap show) toPrint

main :: IO ()
main = do
    putStrLn "Parsing input"
    input <- parseInput  <$> readFile "testinput"
    putStrLn "Solving 1..."
    let solspace1 = step (S.singleton (0,(0,0))) input
    putStr "Solution 1: " >> print solspace1
    putStrLn "Parsing larger input"
    inputLarge <- parseFullInput <$> readFile "input"
    putStrLn "Solving 2..." 
    let solspace2  = step (S.singleton (0,(0,0))) inputLarge
    putStr "Solution 2: " >> print solspace2
    putStrLn "Test"
    return () 
