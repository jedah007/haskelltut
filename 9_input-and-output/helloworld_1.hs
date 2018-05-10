main = putStrLn "hello, world"

-- stack ghc -- --make helloworld.hs

-- putStrLn takes a string and returns an I/O action that
-- has a result type of () (i.e. the empty tuple, also know as unit).
-- The empty tuple is a value of () and it also has a type of ().
