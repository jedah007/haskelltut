-- Reading and writing files

import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- openFile :: FilePath -> IOMode -> IO Handle
-- FilePath is just a type synonym for String
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- hGetContents
-- takes a Handle, so it knows which file to get the contents from
-- and returns an IO String
-- is pretty much like getContents. The only difference is that getContents
-- will automatically read from the standard input (that is from the terminal),
-- whereas hGetContents takes a file handle which tells it
-- which file to read from

-- hClose
-- takes a handle and returns an I/O action that closes the file
