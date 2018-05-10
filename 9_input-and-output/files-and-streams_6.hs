-- Reading and writing files
import System.IO

main = do
    withFile "girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- It takes a path to a file, an IOMode and then it takes a function
-- that takes a handle and returns some I/O action.
-- What it returns is an I/O action that will open that file,
-- do something we want with the file and then close it.
-- The result encapsulated in the final I/O action that's returned
-- is the same as the result of the I/O action
-- that the function we give it returns.


withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result
-- By returning the result encapsulated in the I/O action that we got from f,
-- we make it so that our I/O action encapsulates the same result
-- as the one we got from f handle.


-- Just like we have hGetContents that works like getContents but for a specific file,
-- there's also hGetLine, hPutStr, hPutStrLn, hGetChar, etc.
