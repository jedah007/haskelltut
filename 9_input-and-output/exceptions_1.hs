-- Pure code can throw exceptions, but it they can only be caught in the
-- I/O part of our code (when we're inside a do block that goes into main).
-- That's because you don't know when (or if) anything will be evaluated in
-- pure code, because it is lazy and doesn't have a well-defined order of
-- execution, whereas I/O code does.

-- Don't mix exceptions and pure code. Take advantage of Haskell's powerful
-- type system and use types like Either and Maybe to represent results that
-- may have failed.

-- catch
import System.Environment
import System.IO
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
    | otherwise = ioError e  
