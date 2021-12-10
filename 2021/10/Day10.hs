import Data.List
import Data.Maybe

errorScore :: Char -> Int
errorScore ')' = 3
errorScore ']' = 57
errorScore '}' = 1197
errorScore  _  = 25137

completionScore :: String -> Int
completionScore str = completionScoreHelp str 0
    where 
        completionScoreHelp :: String -> Int -> Int
        completionScoreHelp (x:xs) acc = completionScoreHelp xs (5 * acc + score x)
        completionScoreHelp []     acc = acc
        score :: Char -> Int
        score = (+) 1 . fromJust . flip elemIndex ")]}>"
 
matching :: Char -> Char -> Bool
matching ')' '(' = True 
matching '}' '{' = True
matching ']' '[' = True 
matching '>' '<' = True
matching  _   _  = False

openParen :: Char -> Bool
openParen =  (||) <$> ((||) <$> ((||) <$> (=='(') <*> (=='{')) <*> (=='<')) <*> (=='[')

getErrorScore :: String -> Int
getErrorScore str = errorScoreHelp str []
    where
        errorScoreHelp (x:xs) stack 
            | openParen x             = errorScoreHelp xs (x:stack)
            | matching x (head stack) = errorScoreHelp xs (tail stack)
            | otherwise               = errorScore x
        errorScoreHelp []     _     = 0

isCorrupted :: String -> Bool
isCorrupted = (> 0) . getErrorScore

getCompletionScore :: String -> Int
getCompletionScore str = completionScoreHelp str []
    where
        flipParen :: Char -> Char
        flipParen '(' = ')'
        flipParen '[' = ']'
        flipParen '<' = '>'
        flipParen '{' = '}'
        remainingToCompletion :: String -> String
        remainingToCompletion = map flipParen
        completionScoreHelp :: String -> String -> Int
        completionScoreHelp (x:xs) stack
            | openParen x             = completionScoreHelp xs (x:stack)
            | matching x (head stack) = completionScoreHelp xs (tail stack)
            | otherwise               = error "This should not be happening!"
        completionScoreHelp [] stack  = completionScore . remainingToCompletion $ stack

main :: IO ()
main = do 
    input <- lines <$> readFile "input"
    let solution1 = sum . map getErrorScore $ input
    putStr "Solution 1: " >> print solution1
    let input2 = filter (not . isCorrupted) input
    let solution2List = map getCompletionScore input2
    let midwayPoint   = length solution2List `div` 2
    let solution2 = sort solution2List !! midwayPoint
    putStr "Solution 2: " >> print solution2
    return ()