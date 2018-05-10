main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- To run a program you can either compile it and then run the produced
-- executable file by doing ghc --make helloworld and then ./helloworld
-- or you can use the runhaskell command like so: runhaskell helloworld.hs
-- and your program will be executed on the fly.

-- In Haskell, every if must have a corresponding else
-- because every expression has to have some sort of value.

-- In an I/O do block, ifs have to have a form of:
-- 'if condition then I/O action else I/O action'

-- The return in Haskell is really nothing like the return in most other languages!
-- In Haskell (in I/O actions specifically), it makes an I/O action out of a pure value.

-- Using return DOESN'T cause the I/O do block to end in execution or
-- anything like that. 
