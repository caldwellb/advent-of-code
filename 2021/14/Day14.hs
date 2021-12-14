{-# LANGUAGE ExtendedDefaultRules #-}
import qualified Data.Map.Strict as M
import Data.List.Split

type PolymerChain     = String
type Polymer          = Char
type InsertionRules   = M.Map (Polymer, Polymer) Polymer
type Occurences       = M.Map Polymer Int
type Depth            = Int

parseLine :: String -> ((Polymer,Polymer), Polymer)
parseLine = (,) <$> ((,) <$> head <*> (!! 1)) <*> (!! 6)

parseInput :: String -> (PolymerChain, InsertionRules)
parseInput = ((,) <$> head <*> M.fromList . map parseLine . drop 2) . lines

possiblePolymers :: [Polymer]
possiblePolymers = ['B', 'C', 'F', 'H', 'K', 'N', 'O', 'P', 'S', 'V']

emptyOccurences :: Occurences
emptyOccurences = M.fromList (zip possiblePolymers [0, 0..])

spotPolymer :: Polymer -> Occurences -> Occurences
spotPolymer = flip (M.insertWith (+)) 1

chainStep :: Depth -> InsertionRules -> (Polymer, Polymer) -> Occurences
chainStep 0  rules (x, y) = emptyOccurences
chainStep sn rules (x, y) = spotPolymer (rules M.! (x,y)) (M.unionWith (+) (chainStep (sn - 1) rules (x, rules M.! (x,y))) (chainStep (sn - 1) rules (rules M.! (x,y), y)))

spotTemplate :: Occurences -> PolymerChain -> Occurences
spotTemplate = foldr spotPolymer

main :: IO ()
main = do
    (template, rules) <- parseInput <$> readFile "testinput"
    let first10 = (`spotTemplate` template) . M.unionsWith (+) $ zipWith (curry (chainStep 10 rules)) template (tail template)
    let max10 = maximum first10
    let min10 = minimum first10
    putStr "Solution 1: " >> print (max10 - min10)
    let then40 = (`spotTemplate` template) . M.unionsWith (+) $ zipWith (curry (chainStep 40 rules)) template (tail template)
    let max40 = maximum then40
    let min40 = minimum then40
    putStr "Solution 2: " >> print (max40 - min40)
    return ()