-- getContents
-- is an I/O action that reads everything from the standard input
-- until it encounters an end-of-file character
import Data.Char

main = do
    contents <- getContents
    putStr (map toUpper contents)
