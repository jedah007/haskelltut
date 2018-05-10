-- readFile
-- readFile :: FilePath -> IO String
import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
