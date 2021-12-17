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

parseFullInput :: String -> Array Coordinate Risk
parseFullInput str = let
        doubleList           = map (map digitToInt) . lines $ str
        horizExpand          = foldr1 (zipWith (++)) . map ((\f -> map (map f) doubleList) . incRiskBy) $ [0..4]
        vertExpand           = concatMap ((\f -> map (map f) horizExpand) . incRiskBy) [0..4]
        cols                 = length . head $ vertExpand
        rows                 = length vertExpand
    in listArray ((0,0), (rows - 1, cols - 1)) . concat $ vertExpand

step :: Coordinate -> S.Set Coordinate -> Array Coordinate Int -> Array Coordinate (Int, Bool) -> Array Coordinate Int
step previous tovisit riskarray distarray = let
    bnds                               = bounds distarray
    temp                               = S.map (\x -> (distarray ! x, x))
    ((_,minvalue@(x, y)), resttovisit) = fromJust . S.minView . S.map (\x -> (distarray ! x, x)) $ tovisit
    neighbors                          = filter (not . snd  . (distarray !)) . filter (inRange bnds) $ [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
    updateAt i                         = min (fst (distarray ! minvalue) + riskarray ! i) (fst (distarray ! i))
    stepBy                             =  (minvalue, (fst (distarray ! minvalue), True)) : [ (i, (updateAt i, False)) | i <- neighbors ]
    in if S.null tovisit then fst <$> distarray
        else step minvalue (S.union (S.map snd resttovisit) (S.fromList neighbors)) riskarray (distarray // stepBy)

displayGrid :: Show e  => Array (Int, Int) e -> IO ()
displayGrid arr = let
    columns = snd . snd . bounds $ arr
    toPrint = chunksOf (columns + 1) . elems $ arr
    in putStr . unlines $ map (concatMap show) toPrint

main :: IO ()
main = do
    input <- parseInput  <$> readFile "input"
    let blankarray = listArray (bounds input) (zip (0 : repeat (maxBound - 10)) (True : repeat False))
        solspace1  = step (0,0) (S.singleton (0,0)) input blankarray
    putStr "Solution 1: " >> print (solspace1 ! snd (bounds input))
    inputLarge <- parseFullInput <$> readFile "input"
    let blankLarge = listArray (bounds inputLarge) (zip (0 : repeat (maxBound - 10)) (True : repeat False))
        solspace2  = step (0,0) (S.singleton (0,0)) inputLarge blankLarge
    putStr "Solution 2: " >> print (solspace2 ! snd (bounds inputLarge))
    return ()