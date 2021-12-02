

type SubPos = (Int, Int)

data Direction = Forward Int | Down Int | Up Int
    deriving Show

readDirection :: String -> Direction
readDirection xs = 
    case words xs of
        "forward":x:[] -> Forward $ read x
        "down":x:[]    -> Down    $ read x
        "up":x:[]      -> Up      $ read x
        _              -> error "Improperly formatted input"

changeSubPos :: SubPos -> Direction -> SubPos
changeSubPos (h,v) (Forward x)  = (x + h, v)
changeSubPos (h,v) (Down x)     = (h, x + v)
changeSubPos (h,v) (Up   x)     = (h, max (v - x) 0)

type SubPosAim = (Int, Int, Int)

changeSubPosAim :: SubPosAim -> Direction -> SubPosAim
changeSubPosAim (h,v,a) (Forward x)  = (x + h, (a * x) + v, a)
changeSubPosAim (h,v,a) (Down    x)  = (h, v, a + x)
changeSubPosAim (h,v,a) (Up      x)  = (h, v, a - x)

main :: IO ()
main = do
    inputs <- map readDirection . lines <$> readFile "input"
    putStr "First solution: "
    print . uncurry (*) . foldl changeSubPos (0,0) $ inputs
    putStr "Second solution: "
    print . (\(x,y,_) -> x*y) . foldl changeSubPosAim (0,0,0) $ inputs
