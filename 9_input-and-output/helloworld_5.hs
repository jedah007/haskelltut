-- Before we move on to files, let's take a look at some
-- functions that are useful when dealing with I/O.

-- putStr
main = do   putStr "Hey, "
            putStr "I'm "
            putStrLn "Andy!"

-- putChar
main = do   putChar 't'
            putChar 'e'
            putChar 'h'

-- putStr is actually defined recursively with the help of putChar:
-- putStr :: String -> IO ()
-- putStr [] = return ()
-- putStr (x:xs) = do
--     putChar x
--     putStr xs

-- print
-- takes a value of any type that's an instance of Show
main = do   print True
            print 2
            print "haha"
            print 3.2
            print [3,4,3]

-- getChar
-- is an I/O action that reads a character from the input
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

-- when
-- It takes a boolean value and an I/O action if that boolean value is True,
-- it returns the same I/O action that we supplied to it.
-- However, if it's False, it returns the return (), action,
-- so an I/O action that doesn't do anything.
import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

-- sequence
-- takes a list of I/O actions and returns an I/O actions
-- that will perform those actions one after the other

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
-- Is exactly the same as doing this:
main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs

-- A common pattern with sequence is when we map functions like print
-- or putStrLn over lists.
-- sequence (map print [1,2,3,4,5])
