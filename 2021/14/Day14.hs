{-# LANGUAGE ExtendedDefaultRules #-}
import qualified Data.Map.Strict as M
import Data.List.Split

type PolymerChain     = String
type Polymer          = Char
type InsertionRules   = M.Map (Polymer, Polymer) Polymer
type PairOccurences   = M.Map (Polymer, Polymer) Int
type Occurences       = M.Map Polymer Int
type Depth            = Int

parseLine :: String -> ((Polymer,Polymer), Polymer)
parseLine = (,) <$> ((,) <$> head <*> (!! 1)) <*> (!! 6)

parseInput :: String -> (PolymerChain, InsertionRules)
parseInput = ((,) <$> head <*> M.fromList . map parseLine . drop 2) . lines

possiblePolymers :: [Polymer]
possiblePolymers = ['B', 'C', 'F', 'H', 'K', 'N', 'O', 'P', 'S', 'V']


polymerPairs :: [(Polymer, Polymer)]
polymerPairs = [ (a, b) | a <- possiblePolymers, b <- possiblePolymers ]

emptyOccurences :: PairOccurences
emptyOccurences = M.fromList (zip polymerPairs [0, 0..])

spotPolymers :: (Polymer, Polymer) -> PairOccurences -> PairOccurences
spotPolymers = flip (M.insertWith (+)) 1

spotPolymer :: Polymer -> Occurences -> Occurences
spotPolymer = flip (M.insertWith (+)) 1

splitPolymerVal :: ((Polymer,Polymer), Int) -> InsertionRules -> PairOccurences
splitPolymerVal ((x, y), val) rules = let newPolymer = rules M.! (x, y) in M.fromList [((x, newPolymer), val), ((newPolymer, y), val)]

chainStep :: InsertionRules -> PairOccurences -> PairOccurences 
chainStep rules = M.unionsWith (+) . map (`splitPolymerVal` rules) . M.toAscList 

countPolymers :: PairOccurences -> Polymer -> Polymer -> Occurences
countPolymers occ p1 p2 = M.map (`div` 2) . spotPolymer p1 . spotPolymer p2 . M.fromListWith (+) . foldr (\((x, y), val) acc -> (x, val) : (y, val) : acc) [] . M.toAscList $ occ

main :: IO ()
main = do
    (template, rules) <- parseInput <$> readFile "input"
    let undercount1        = head template
        undercount2        = last template 
        templateOccurences = foldr spotPolymers emptyOccurences (zip template $ tail template)
        tenRuns            = iterate (chainStep rules) templateOccurences !! 10
        solution1          = countPolymers tenRuns undercount1 undercount2
        max1               = maximum solution1
        min1               = minimum solution1
    putStr "Solution 1: " >> print (max1 - min1)
    let fortyRuns          = iterate (chainStep rules) tenRuns !! 30
        solution2          = countPolymers fortyRuns undercount1 undercount2
        max2               = maximum solution2
        min2               = minimum solution2
    putStr "Solution 2: " >> print (max2 - min2)