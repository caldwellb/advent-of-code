{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Char
import qualified Text.Parsec as P
import Control.Monad.Identity
import Data.Foldable
import Data.Array.IO

{- Basic construction of a line, checking for straightness, unfolding a straight line -}

data Line = L2 (Int, Int) (Int, Int)
    deriving (Show, Eq)

type Lines = [Line]

straightLine :: Line -> Bool
straightLine (L2 (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

onlyStraight :: Lines -> Lines
onlyStraight = filter straightLine

notStraight :: Lines -> Lines
notStraight = filter (not . straightLine)

unfoldStraightLine :: Line -> [(Int, Int)]
unfoldStraightLine (L2 (x1,y1) (x2,y2))
    | x1 == x2  = [ (x1, ys) | ys <- [(min y1 y2) .. (max y1 y2)] ]
    | y1 == y2  = [ (xs, y1) | xs <- [(min x1 x2) .. (max x1 x2)] ]
    | otherwise = error "Non straight line handed to unfoldStraightLine"



unfoldDiagonalLine :: Line -> [(Int,Int)]
unfoldDiagonalLine (L2 (x1,y1) (x2,y2)) =let
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2 in
        doUnfold (minX, minY) maxX
        where
            doUnfold :: (Int, Int) -> Int -> [(Int,Int)]
            doUnfold (x, y) maxX
                | x == maxX  = [(x,y)]
                | otherwise = (x,y) : doUnfold (x + 1, y + 1) maxX
                


{- Parsing lines to input -}

arrow :: P.Stream s Identity Char => P.Parsec s u String
arrow = P.string " -> "

comma :: P.Stream s Identity Char => P.Parsec s u Char
comma = P.char ','

grabNumber :: P.Stream s Identity Char => P.Parsec s u Int
grabNumber = read <$> P.many (P.satisfy isDigit)

parseCommaNumbers :: P.Stream s Identity Char => P.Parsec s u (Int, Int)
parseCommaNumbers = (,) <$> grabNumber <*> (comma *> grabNumber)

formatInput :: P.Stream s Identity Char => P.Parsec s u Line
formatInput = 
    L2 <$> parseCommaNumbers <*> (arrow *> parseCommaNumbers)

parseLine :: String -> Line
parseLine xs = case P.parse formatInput "" xs of
    Left  err     ->  error "Parse failed"
    Right success -> success

getInputLines, getTestInputs :: IO Lines
getInputLines = map parseLine . lines <$> readFile "input"
getTestInputs = map parseLine . lines <$> readFile "testinput"

{- Main loop -}

main :: IO ()
main = do
    grid   <- newArray ((0,0),(999,999)) 0 :: IO (IOArray (Int,Int) Int)
    inputs <- getInputLines
    let straightInputs = onlyStraight inputs
    for_ straightInputs $ \line -> do
        let points = unfoldStraightLine line
        for_ points $ \point -> do
            currentVal <- readArray grid point
            writeArray grid point (currentVal + 1)
    elems1 <- getElems grid
    putStr "Solution 1: " >> print (length . filter (> 1) $ elems1)
    let diagonalInputs = notStraight inputs
    for_ diagonalInputs $ \line -> do
        let points = unfoldDiagonalLine line
        for_ points $ \point -> do
            currentVal <- readArray grid point
            writeArray grid point (currentVal + 1)
    elems2 <- getElems grid
    putStr "Solution 2: " >> print (length . filter (> 1) $ elems2)
    return ()

test:: IO ()
test = do
    grid   <- newArray ((0,0),(9,9)) 0 :: IO (IOArray (Int,Int) Int)
    inputs <- getInputLines
    let straightInputs = onlyStraight inputs
    for_ straightInputs $ \line -> do
        let points = unfoldStraightLine line
        for_ points $ \point -> do
            currentVal <- readArray grid point
            writeArray grid point (currentVal + 1)
    elems1 <- getElems grid
    putStr "Solution 1: " >> print (length . filter (> 1) $ elems1)
    let diagonalInputs = notStraight inputs
    for_ diagonalInputs $ \line -> do
        let points = unfoldDiagonalLine line
        for_ points $ \point -> do
            currentVal <- readArray grid point
            writeArray grid point (currentVal + 1)
    elems2 <- getElems grid
    putStr "Solution 2: " >> print (length . filter (> 1) $ elems2)
    return ()