-- System.Random
-- random :: (RandomGen g, Random a) => g -> (a, g)

-- ghci> random (mkStdGen 949488) :: (Float, StdGen)
-- (0.8938442,1597344447 1655838864)
-- ghci> random (mkStdGen 949488) :: (Bool, StdGen)
-- (False,1485632275 40692)
-- ghci> random (mkStdGen 949488) :: (Integer, StdGen)
-- (1691547873,1597344447 1655838864)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)
-- We didn't have to do random gen :: (Bool, StdGen).
-- That's because we already specified that we want booleans
-- in the type declaration of the function.


-- So what if we want to flip four coins? Or five?
-- Well, there's a function called randoms that takes a generator
-- and returns an infinite sequence of values based on that generator.
-- ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen


finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)


-- What if we want a random value in some sort of range?
-- randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)
-- ghci> randomR (1,6) (mkStdGen 359353)

-- There's also randomRs, which produces a stream of random values
-- within our defined ranges.
-- ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
