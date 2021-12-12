import qualified Data.Map as M
import Data.IORef ( modifyIORef', newIORef, readIORef, IORef )
import Data.List.Split ( splitOn )
import Data.Char ( isLower )
import Data.List (sortOn)
import Data.Bifunctor ( Bifunctor(first) )

type Name = String
data Node = Small Name Bool | Large Name | Start | End
    deriving (Eq, Ord)

instance Show Node where
    show (Small str True)  = str
    show (Small str False) = str
    show (Large str)       = str
    show Start = "start"
    show End = "end"

type Nodes = [Node]
type Cave = (M.Map Name Nodes, Bool)

decideNode :: String -> Node
decideNode "start" = Start
decideNode "end"   = End
decideNode str = if all isLower str then Small str False else Large str

parseLine :: IORef (M.Map String Nodes) -> String -> IO ()
parseLine cave str = let 
    split  = splitOn "-" str
    left  = decideNode $ head split
    right = decideNode $ last split
    in do 
        modifyIORef' cave (M.insertWith (<>) (head split) [right])
        modifyIORef' cave (M.insertWith (<>) (last split) [left])

markVisited :: Node -> Cave -> Cave
markVisited n@(Small str False) cave = first (M.map (map (\x -> if x == n then Small str True else x))) cave
markVisited n@(Small str True) (cave, False) = (cave, True)
markVisited n@(Small str True) (cave, True) = (cave, True)
markVisited _ cave                   = cave

validCaves :: Node -> Bool
validCaves Start            = False 
validCaves (Small _ True)   = False
validCaves _                = True

validCaves2 :: Node -> Bool
validCaves2 Start = False
validCaves2 _     = True

makePaths :: Node -> Cave -> [Nodes]
makePaths End _   = [[End]]
makePaths Start cave     = map (Start:) $ concatMap (`makePaths` cave) (fst cave M.! "start")
makePaths n@(Small name visited) toUpdate =
    let c@(cave, twovisits) = markVisited n toUpdate in
        if twovisits then
            let neighbors = filter validCaves $ cave M.! name in
                map (n : ) $ concatMap (`makePaths` c) neighbors
        else
            let neighbors = filter validCaves2 $ cave M.! name in
                map (n : ) $ concatMap (`makePaths` c) neighbors 
makePaths n@(Large name) c@(cave, twovisits) = 
    if twovisits then
        let neighbors = filter validCaves $ cave M.! name in
            map (n : ) $ concatMap (`makePaths` c) neighbors
    else
        let neighbors = filter validCaves2 $ cave M.! name in
            map (n : ) $ concatMap (`makePaths` c) neighbors 

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    caveRef <- newIORef M.empty
    mapM_ (parseLine caveRef) input
    cave <- readIORef caveRef
    let paths1 = makePaths Start (cave, True)
    let paths = makePaths Start (cave, False)
    putStr "Solution 1: " >> print (length paths1)
    putStr "Solution 2: " >> print (length paths)
    return ()