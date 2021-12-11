import Data.Array
import Data.Array.MArray
import Data.Char
import Data.Foldable
import Data.IORef
import Control.Monad
import Data.List

type Coordinate = (Int, Int)
type Height     = Int
type Flow       = Int
type Heightmap  = Array Coordinate Height
type Flowmap    = Array Coordinate (Height, Flow)

testInput :: String
testInput = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

makeHeightmap :: String -> Heightmap
makeHeightmap str = let
    lineSplit  = lines str
    lineLength = length . head $ lineSplit
    numberCols = length lineSplit
    finalStr   = concatMap (map digitToInt) lineSplit
    in listArray ((0,0), (numberCols - 1, lineLength - 1)) finalStr

neighborhood :: Coordinate -> [Coordinate]
neighborhood (x,y) = [(x,y), (x - 1,y), (x + 1,y), (x,y - 1), (x,y + 1)]

defaultAccess :: Heightmap -> Coordinate -> Height
defaultAccess hm coord = if inRange (bounds hm) coord then hm ! coord else 10

(<!) :: Heightmap -> Coordinate -> Height
(<!) = defaultAccess

defaultAccessFlow :: Flowmap -> Coordinate -> (Height, Flow)
defaultAccessFlow fm coord = if inRange (bounds fm) coord then fm ! coord else (10, 0)

(<!!) :: Flowmap -> Coordinate -> (Height, Flow)
(<!!) = defaultAccessFlow

infixr 7 <!

isLowPoint :: Heightmap -> Coordinate -> Bool
isLowPoint hm (x,y) = all (> hm ! (x,y)) neighbors
    where neighbors = map (hm <!) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

heightToFlow :: Heightmap -> Flowmap
heightToFlow = (applyFlow <$>)
    where 
        applyFlow :: Height -> (Height, Flow)
        applyFlow 9 = (9,0)
        applyFlow x = (x,1)

smallestNeighbor :: Flowmap -> Coordinate -> Coordinate
smallestNeighbor fm coord = foldl foldFunct coord (neighborhood coord)
    where
        foldFunct :: Coordinate -> Coordinate -> Coordinate
        foldFunct x y = if fst (fm <!! x) < fst (fm <!! y) then x else y

isSmallestNeighborOf :: Flowmap -> Coordinate -> Coordinate -> Bool
isSmallestNeighborOf fm c0 c1 = smallestNeighbor fm c1 == c0


flowStepLocal :: Flowmap -> Coordinate -> (Height, Flow)
flowStepLocal fm coord = let
    -- neighbors = map (fm <!!) (neighborhood coord)
    incoming  = filter (isSmallestNeighborOf fm coord) (neighborhood coord)
    flowIn    = sum $ map (snd . (fm <!!)) incoming
    height    = fst (fm ! coord)
    in (height, flowIn)

flowStep :: Flowmap -> Flowmap
flowStep fm = listArray (bounds fm) $ map (flowStepLocal fm) (indices fm)

flowStopped :: Flowmap -> Bool
flowStopped = (==) <$> id <*> flowStep

flowSim :: Flowmap -> Flowmap
flowSim = until flowStopped flowStep

main :: IO ()
main = do
    hm    <- makeHeightmap <$> readFile "input"
    accum <- newIORef 0
    for_ (indices hm) $ \coord -> 
        when (isLowPoint hm coord) (modifyIORef' accum (hm ! coord + 1 +))
    solution1 <- readIORef accum
    putStr "Solution 1: " >> print solution1
    let fm = heightToFlow hm
    let solution2 = product . take 3 . reverse . sort . map snd . elems $ flowSim fm
    putStr "Solution 2: " >> print solution2
    return ()
