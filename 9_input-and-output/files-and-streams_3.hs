-- interact
-- takes a function of type String -> String as a parameter and returns
-- an I/O action that will take some input, run that function on it and
-- then print out the function's result.
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

--shorter:
-- main = interact $ unlines . filter ((<10) . length) . lines
