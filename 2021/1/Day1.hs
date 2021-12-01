import Data.List
import Data.IORef
import Data.Foldable
import Control.Monad

readFileLines :: String -> IO [Int]
readFileLines filename = do
    input <- readFile filename
    return (read <$> lines input)

compareFirstTwo :: [Int] -> Bool
compareFirstTwo (x:y:zs) = y > x
compareFirstTwo _        = False

compareFirstAndFour :: [Int] -> Bool
compareFirstAndFour (x:y:z:a:bs) = a > x
compareFirstAndFour _            = False

main :: IO ()
main = do
    -- Reads in the inputs, will be used multiple times.
    inputs <- readFileLines "input"
    -- Initializes a counter to check each of the values
    counter <- newIORef 0
    -- Loops over the possible tails of the inputs, comparing the first two
    -- elements. This is the fastest way I know how to access every first few
    -- elements of a list.
    for_ (tails inputs) (\lst -> do
        when (compareFirstTwo lst) 
             $ modifyIORef' counter (+1))
    firstSolution <- readIORef counter
    putStr "Part 1: "
    print firstSolution
    -- Resets IO counter
    writeIORef counter 0
    -- modifyIORef' strictly evaluates the reference, meaning no data overhead
    -- in our lazy language! yay!
    for_ (tails inputs) (\lst -> do
        when (compareFirstAndFour lst)
            $ modifyIORef' counter (+1))
    secondSolution <- readIORef counter
    putStr "Part 2: "
    print secondSolution