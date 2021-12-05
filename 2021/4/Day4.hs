import Data.Array
import Data.List
import Data.List.Split

type BingoBoard = [[(Int, Bool)]]

-- Given a list of 5 strings (the rows of a bingo board) make a bingo board.
makeBingoBoard :: [String] -> BingoBoard
makeBingoBoard = map (map (\x -> (read x, False)) . words)

-- Properly take in the input and format it into something we can use.
formatInput :: [String] -> ([Int], [BingoBoard])
formatInput (ballsDrawn : spacer : bingoBoards) = 
    (map read . splitOn "," $ ballsDrawn, map makeBingoBoard . splitOn [""] $ bingoBoards)

readInputs, readTestInputs :: IO ([Int], [BingoBoard])
readInputs     = formatInput . lines <$> readFile "input"
readTestInputs = formatInput . lines <$> readFile "testinput"

unmarkedSum :: BingoBoard -> Int
unmarkedSum =  sum . map fst . filter (not . snd) . concat

winningRow :: BingoBoard -> Bool
winningRow = any (all snd)

winningColumn :: BingoBoard -> Bool
winningColumn = winningRow . transpose

isWinner :: BingoBoard -> Bool
isWinner = (||) <$> winningRow <*> winningColumn

checkWinner :: [BingoBoard] -> Maybe BingoBoard
checkWinner []     = Nothing
checkWinner (x:xs) = if winningRow x || winningColumn x then Just x else checkWinner xs

markBoards :: Int -> [BingoBoard] -> [BingoBoard]
markBoards mark = map (markBoard mark)
    where
        markBoard :: Int -> BingoBoard -> BingoBoard
        markBoard mark = map (map (markSquare mark))
        markSquare :: Int -> (Int, Bool) -> (Int, Bool)
        markSquare x (y, b) = if x == y then (x, True) else (y, b)


mainLoop1 :: [Int] -> [BingoBoard] -> IO Int
mainLoop1 [] _ = error "Reached end of Bingo with no winners"
mainLoop1 (current:rest) boards = do
    let newBoards = markBoards current boards
    case checkWinner newBoards of
        Just winningBoard -> return (current * unmarkedSum winningBoard)
        Nothing           -> mainLoop1 rest newBoards

mainLoop2 :: [Int] -> [BingoBoard] -> IO Int
mainLoop2 [] _ = error "Reached the end of Bingo with multiple non-winners."
mainLoop2 (current:rest) boards = do
    let newBoards = markBoards current boards
    case newBoards of
        [finalBoard] -> if isWinner finalBoard 
                        then return (current * unmarkedSum finalBoard) 
                        else mainLoop2 rest [finalBoard] 
        _ -> mainLoop2 rest (filter (not . isWinner) newBoards)

main :: IO ()
main = do
    (balls, boards) <- readInputs
    winningVal      <- mainLoop1 balls boards
    putStr "Solution 1: " >> print winningVal
    losingVal       <- mainLoop2 balls boards
    putStr "Solution 2: " >> print losingVal