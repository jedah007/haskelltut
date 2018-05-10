-- appendFile
-- appendFile :: FilePath -> String -> IO ()

-- doesn't truncate the file to zero length if it already exists
-- but it appends stuff to it.

import System.IO

main = do
    todoItem <- getLine
    appendFile "todo.txt" (todoItem ++ "\n")
-- We needed to add the "\n" to the end of each line
-- because getLine doesn't give us a newline character at the end.
