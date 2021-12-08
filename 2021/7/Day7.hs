
testInput :: [Int]
testInput = [16,1,2,0,4,2,7,1,2,14]

solve1 :: [Int] -> Int
solve1 lst = minimum . map (sum . flip map lst) $ distance <$> [1..maximum lst]
    where distance x y = abs (x - y)

solve2 :: [Int] -> Int
solve2 lst = minimum . map (sum . flip map lst) $ distance <$> [1..maximum lst]
    where distance x y = (abs (x - y) * (abs (x - y) + 1)) `div` 2

main :: IO ()
main = do
    input <- readFile "input"
    let crabList = read ('[':input ++ "]") :: [Int]
    putStr "Solution 1: " >> print (solve1 crabList)
    putStr "Solution 2: " >> print (solve2 crabList)
    return ()