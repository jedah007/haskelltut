-- Another way is to use the newStdGen action, which splits our current
-- random generator into two generators. It updates the global random generator
-- with one of them and encapsulates the other as its result.

import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen')
