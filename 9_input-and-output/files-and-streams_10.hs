-- Control how exactly buffering is done

main = do
    withFile "something.txt" ReadMode (\handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)

-- BufferMode is a simple enumeration data type
-- and the possible values it can hold are:
-- NoBuffering, LineBuffering or BlockBuffering (Maybe Int)
-- The Maybe Int is for how big the chunk should be, in bytes.
