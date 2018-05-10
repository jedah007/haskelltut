main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
-- getLine is an I/O action that contains a result type of String
-- getLine is in a sense impure because its result value is
-- not guaranteed to be the same when performed twice.

-- Every I/O action that gets performed has a result encapsulated within it.
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")
-- However, foo would just have a value of ().
-- Notice that we didn't bind the last putStrLn to anything.
-- That's because in a do block, the last action cannot be bound to a name!

-- To get the value out of an I/O action, you have to perform it inside
-- another I/O action by binding it to a name with '<-'.
