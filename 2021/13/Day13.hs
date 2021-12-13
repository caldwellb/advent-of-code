import qualified Data.Set as S
import Data.Bifunctor ( Bifunctor(second, first, bimap) )
import Data.List.Split
import qualified Data.IntMap as Set

type Point = (Int, Int) 
type Paper = S.Set Point
type Fold  = Paper -> Paper

parsePoint :: String -> Point
parsePoint str = read $ '(':str ++ ")"

parseFolds :: String -> Fold
parseFolds ('f':xs) = case drop 10 xs of
    'x':'=':num -> foldX (read num)
    'y':'=':num -> foldY (read num)
    [] -> error "Improperly parsed fold command"
parseFolds _ = error "Not a fold command"

foldInt :: Int -> Int -> Int
foldInt x at
    | x > at    = 2 * at - x
    | otherwise = x

foldX, foldY :: Int -> Fold
foldX at = S.map (first  (`foldInt` at))
foldY at = S.map (second (`foldInt` at))

xOrDot :: Paper -> Point -> Char
xOrDot points point = if point `S.member` points then '■' else ' '

showPaper :: Paper -> IO ()
showPaper points = let
    xmax      = maximum $ S.map fst points
    ymax      = maximum $ S.map snd points
    col y     = [ (x,y) | x <- [0..xmax]]
    grids     = [ col y | y <- [0..ymax]]
    lineToStr = map (xOrDot points)
    in  
        mapM_ (putStrLn . lineToStr) grids


main :: IO ()
main = do
    (points, folds) <- bimap (S.fromList . map parsePoint) (map parseFolds) . (\xs -> (head xs, last xs)) . take 2 . splitOn [""] . lines <$> readFile "input"
    let firstFold = head folds
    let solution1 = length $ firstFold points
    putStr "Solution 1: " >> print solution1
    let totalFold = foldl1 (.) (reverse folds)
    showPaper (totalFold points)
    return ()