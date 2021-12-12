{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.IORef
import Data.List.Split
import Data.Char
import Data.List
import qualified Data.Set as Set

data Node = Small String Int | Large String | Start | End
    deriving (Eq, Ord)

instance Show Node where
    show (Small str _)  = str
    show (Large str)       = str
    show Start = "start"
    show End = "end"

type Nodes = [Node]
type Cave = M.Map Node Nodes

decideNode :: String -> Node
decideNode "start" = Start
decideNode "end"   = End
decideNode str = if all isLower str then Small str 2 else Large str

parseLine :: IORef Cave -> String -> IO ()
parseLine cave str = let 
    split  = splitOn "-" str
    first  = decideNode $ head split
    second = decideNode $ last split
    in do 
        modifyIORef' cave (M.insertWith (<>) first [second])
        modifyIORef' cave (M.insertWith (<>) second [first])

smallZero :: Node -> Node
smallZero (Small str x) = Small str (x - 1)
smallZero x = x

smallTwo :: Node -> Bool
smallTwo (Small _ 2) = True
smallTwo _ = False

markVisited :: Node -> Cave -> Cave
markVisited n@(Small str 2) cave = M.map (map (\x -> if x == n then Small str 1 else x)) cave
markVisited n@(Small str 1) cave = if any (any smallTwo) cave then M.map (map smallZero) cave else M.map (map (\x -> if x == n then Small str 0 else x)) cave
markVisited _ cave               = cave

isSmall :: Node -> Bool 
isSmall (Small _ _) = True
isSmall _           = False

validCaves :: Node -> Bool
validCaves Start            = False 
validCaves (Small _ val)    = val > 0
validCaves _                = True

makePaths :: Node -> Cave -> [Nodes]
makePaths End _   = [[End]]
makePaths Start cave = map (Start:) $ concatMap (`makePaths` cave) (cave M.! Start)
makePaths n@(Small str x) cave = let
    neighbors   = filter validCaves $ cave M.! Small str 2
    updatedCave = markVisited n cave 
    in map (Small str 0:) $ concatMap (`makePaths` updatedCave) neighbors
makePaths n@(Large _) cave   = let
    neighbors = filter validCaves $ cave M.! n
    in map (n:) $ concatMap (`makePaths` cave) neighbors

main :: IO ()
main = do
    input <- lines <$> readFile "testinput"
    caveRef <- newIORef M.empty
    mapM_ (parseLine caveRef) input
    cave <- readIORef caveRef
    let paths = sort $ makePaths Start cave
    mapM_ print paths
    putStr "Solution 2: " >> print (length paths)
    return ()