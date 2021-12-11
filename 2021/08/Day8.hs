import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Tuple
import Data.Char (intToDigit)

isOne, isFour, isSeven, isEight, lengthDecide :: String -> Bool
isOne   = (== 2) . length
isFour  = (== 4) . length
isSeven = (== 3) . length
isEight = (== 7) . length
lengthDecide = (||) <$> isOne <*> ((||) <$> isFour <*> ( (||) <$> isSeven <*> isEight))

solve1Line :: String -> Int
solve1Line = length . filter lengthDecide . words . (!! 1) . splitOn "|"

solve1 :: [String] -> Int
solve1 = sum . map solve1Line

testLine :: String
testLine  = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
testInput, testSixes, testFives :: [String]
testInput = words "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb"
testSixes = filter ((== 6) . length) testInput
testFives = filter ((== 5) . length) testInput

type SevenSegment = String 
type SolutionSet  = Map.Map SevenSegment Int
type Solver       = Map.Map Int SevenSegment

solverToSolution :: Solver -> SolutionSet
solverToSolution = Map.fromList . map swap . Map.toAscList

initializeSolver :: [SevenSegment] -> Solver
initializeSolver = Map.fromList . zip [1,7,4,8] . sortOn length . filter lengthDecide

overlaps :: SevenSegment -> SevenSegment -> Bool
overlaps x y = y `isSubsequenceOf` x

solveForSixes :: Solver -> [SevenSegment] -> Solver
solveForSixes solver [] = solver
solveForSixes solver (seg:rest)
    | not $ seg `overlaps` (solver Map.! 1) = solveForSixes (Map.insert 6 seg solver) rest
    | not $ seg `overlaps` (solver Map.! 4) = solveForSixes (Map.insert 0 seg solver) rest
    | otherwise                             = solveForSixes (Map.insert 9 seg solver) rest

solveForFives :: Solver -> [SevenSegment] -> Solver
solveForFives solver [] = solver
solveForFives solver (seg:rest)
    | (solver Map.! 6) `overlaps` seg = solveForFives (Map.insert 5 seg solver) rest
    | seg `overlaps` (solver Map.! 7) = solveForFives (Map.insert 3 seg solver) rest
    | otherwise                       = solveForFives (Map.insert 2 seg solver) rest

inputToSolution :: [SevenSegment] -> SolutionSet
inputToSolution segs = let
    initial   = initializeSolver segs
    thenSixes = solveForSixes initial   (filter ((== 6) . length) segs)
    thenFives = solveForFives thenSixes (filter ((== 5) . length) segs)
    in solverToSolution thenFives

solveOutput :: SolutionSet -> [SevenSegment] -> [Int]
solveOutput solutions = map (solutions Map.!)

solve2Line :: String -> Int
solve2Line line = let
    [input, output] = map words . take 2 $ splitOn "|" line
    in 
        read (map intToDigit $ solveOutput (inputToSolution (map sort input)) (map sort output))

solve2 :: [String] -> Int
solve2 = sum . map solve2Line

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    putStr "Solution 1: " >> print (solve1 input)
    putStr "Solution 2: " >> print (solve2 input)
        