{-# LANGUAGE ExtendedDefaultRules #-}
import qualified Data.Map as M
import Data.List.Split
import Data.Array as SA
import Data.Array.IO
import Data.IORef
import Control.Monad
import qualified Data.Text as T
default (T.Text)

type PolymerChain     = String
type Polymer          = Char
type InsertionRules   = M.Map (Polymer, Polymer) Polymer
type OccurenceCounter = IOArray Polymer Int

parseLine :: String -> ((Polymer,Polymer), Polymer)
parseLine = (,) <$> ((,) <$> head <*> (!! 1)) <*> (!! 6)

parseInput :: String -> (PolymerChain, InsertionRules)
parseInput = ((,) <$> head <*> M.fromList . map parseLine . drop 2) . lines

chainStep :: OccurenceCounter -> InsertionRules -> PolymerChain -> IO PolymerChain
chainStep ctr pairInsert (x:y:zs) = do
    let newPolymer = pairInsert M.! (x, y)
    increaseOccurenceCounter ctr newPolymer
    others <- chainStep ctr pairInsert (y:zs)
    return (x : newPolymer : others)
chainStep ctr pairInsert chain = return chain

possiblePolymers :: [Polymer]
possiblePolymers = ['B', 'C', 'F', 'H', 'K', 'N', 'O', 'P', 'S', 'V']

countOf :: Polymer -> PolymerChain -> Int
countOf x = length . filter (== x)

highestCountPolymer :: PolymerChain -> Int
highestCountPolymer chain = maximum $ (`countOf` chain) <$> possiblePolymers

lowestCountPolymer :: PolymerChain -> Int
lowestCountPolymer chain = minimum . filter (/= 0) $ (`countOf` chain) <$> possiblePolymers 

increaseOccurenceCounter :: OccurenceCounter -> Polymer -> IO ()
increaseOccurenceCounter counter polymer = readArray counter polymer >>= writeArray counter polymer . (1+)

makeOccurenceCounter :: PolymerChain -> IO OccurenceCounter
makeOccurenceCounter chain = do
    blankArray <- newArray ('B', 'V') 0 
    mapM_ (blankArray `increaseOccurenceCounter`) chain
    return blankArray

main :: IO ()
main = do
    (template, rules) <- parseInput <$> readFile "input"
    ctr <- makeOccurenceCounter template
    ref <- newIORef template
    forM_ ([1..10] :: [Int]) $ \_ -> do
        readIORef ref >>= chainStep ctr rules >>= writeIORef ref
    maxVal <- maximum <$> (freeze ctr :: IO (Array Polymer Int))
    minVal <- foldl1 (\x y -> if y == 0 then x else min x y) <$> (freeze ctr :: IO (Array Polymer Int))
    putStr "Solution 1: " >> print (maxVal - minVal)
    forM_ ([11..40] :: [Int]) $ \_ -> do
        readIORef ref >>= chainStep ctr rules >>= writeIORef ref
    maxVal <- maximum <$> (freeze ctr :: IO (Array Polymer Int))
    minVal <- foldl1 (\x y -> if y == 0 then x else min x y) <$> (freeze ctr :: IO (Array Polymer Int))
    return ()
