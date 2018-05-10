-- System.Random offers the getStdGen I/O action, which has a type of IO StdGen.
-- When your program starts, it asks the system for a good random number generator
-- and stores that in a so called global generator. getStdGen fetches you that
-- global random generator when you bind it to something.

import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)

-- Be careful though, just performing getStdGen twice will ask the system
-- for the same global generator twice.
