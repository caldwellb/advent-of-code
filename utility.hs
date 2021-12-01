
readFileLines :: Read a => String -> IO [a]
readFileLines filename = do
    input <- readFile filename
    return (read <$> lines input)