import Data.List.Split

data School = FishAges Int Int Int Int Int Int Int Int Int

age :: School -> School
age (FishAges x0 x1 x2 x3 x4 x5 x6 x7 x8) = FishAges x1 x2 x3 x4 x5 x6 (x0 + x7) x8 x0

population :: School -> Int
population (FishAges x0 x1 x2 x3 x4 x5 x6 x7 x8) = x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8

howManyFishAfterN :: Int -> School -> Int
howManyFishAfterN reps fish = population $ iterate age fish !! reps

addFish :: Int -> School -> School
addFish x sch@(FishAges x0 x1 x2 x3 x4 x5 x6 x7 x8)
    | x == 0 = FishAges (1 + x0) x1 x2 x3 x4 x5 x6 x7 x8
    | x == 1 = FishAges x0 (1 + x1) x2 x3 x4 x5 x6 x7 x8
    | x == 2 = FishAges x0 x1 (1 + x2) x3 x4 x5 x6 x7 x8
    | x == 3 = FishAges x0 x1 x2 (1 + x3) x4 x5 x6 x7 x8
    | x == 4 = FishAges x0 x1 x2 x3 (1 + x4) x5 x6 x7 x8
    | x == 5 = FishAges x0 x1 x2 x3 x4 (1 + x5) x6 x7 x8
    | x == 6 = FishAges x0 x1 x2 x3 x4 x5 (1 + x6) x7 x8
    | x == 7 = FishAges x0 x1 x2 x3 x4 x5 x6 (1 + x7) x8
    | x == 8 = FishAges x0 x1 x2 x3 x4 x5 x6 x7 (1 + x8)
    | otherwise = sch

sortFish :: [Int] -> School
sortFish = foldr addFish (FishAges 0 0 0 0 0 0 0 0 0)

main :: IO ()
main = do
    inputs <- sortFish . map read . splitOn "," <$> readFile "input"
    putStr "Solution 1: "
    print $ howManyFishAfterN 80 inputs
    putStr "Solution 2: "
    print $ howManyFishAfterN 256 inputs
    return ()