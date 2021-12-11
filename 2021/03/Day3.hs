import Data.List ( transpose )
import Data.Char ( digitToInt )

testInput2 = map (map digitToInt) ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

gammaRateBit :: [Int] -> Int
gammaRateBit xs = if length (filter (== 1) xs) > (length xs `div` 2) then 1 else 0

epsilonRateBit :: [Int] -> Int
epsilonRateBit = flip . gammaRateBit
    where 
        flip 0 = 1
        flip 1 = 0

decodeBinary :: [Int] -> Int
decodeBinary = decodeBinaryRec 0
    where
        decodeBinaryRec :: Int -> [Int] -> Int
        decodeBinaryRec acc []     = acc
        decodeBinaryRec acc (x:xs) = decodeBinaryRec ((2 * acc) + x) xs

gammaRate :: [[Int]] -> Int
gammaRate = decodeBinary . map gammaRateBit

epsilonRate :: [[Int]] -> Int
epsilonRate = decodeBinary . map epsilonRateBit

headIs :: Eq a => [a] -> a -> Bool
headIs (x:xs) query = x == query
headIs []     _     = False

headOne, headZero  :: [Int] -> Bool
headOne  = (`headIs` 1)
headZero = (`headIs` 0)

majorityHeads, minorityHeads :: [[Int]] -> Int
majorityHeads xss = let
    oneCount  = length $ filter headOne  xss
    zeroCount = length $ filter headZero xss
    in if zeroCount > oneCount then 0 else 1
minorityHeads xss = let
    oneCount  = length $ filter headOne  xss
    zeroCount = length $ filter headZero xss
    in if oneCount >= zeroCount then 0 else 1

queryAll :: ([[Int]] -> Int) -> [[Int]] -> [Int] 
queryAll _ []  = []
queryAll _ [x] = x
queryAll query xss = let
    queryResult        = query xss
    queryPossibilities = filter (`headIs` queryResult) xss
    in
        queryResult : queryAll query (map tail queryPossibilities)

main :: IO ()
main = do
    input1 <- map (map digitToInt) . transpose . lines <$> readFile "input"
    let inputGamma   = gammaRate   input1
    let inputEpsilon = epsilonRate input1
    putStr "Solution 1: " >> print (inputGamma * inputEpsilon)
    input2 <- map (map digitToInt) . lines <$> readFile "input"
    let oxygenGenRating = decodeBinary $ queryAll majorityHeads input2
    let carbonScrubber  = decodeBinary $ queryAll minorityHeads input2
    putStr "Solution 2: " >> print (oxygenGenRating * carbonScrubber)