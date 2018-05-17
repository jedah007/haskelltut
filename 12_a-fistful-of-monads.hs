-- Monads are a natural extension of applicative functors and with them
-- we're concerned with this: if you have a value with a context, m a,
-- how do you apply to it a function that takes a normal a and returns a
-- value with a context? That is, how do you apply a function of
-- type a -> m b to a value of type m a? So essentially,
-- we will want this function:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
--
-- The >>= function is pronounced as 'bind'.

-- If we have a fancy value and a function that takes a normal value
-- but returns a fancy value, how do we feed that fancy value into the function?

-- Getting our feet wet with Maybe

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f  = Nothing
applyMaybe (Just x) f = f x
--
-- ghci> Just 3 `applyMaybe` \x -> Just (x+1)
-- Just 4


-- The Monad type class

--class Monad m where
--   return :: a -> m a
--
--   (>>=) :: m a -> (a -> m b) -> m b
--
--   (>>) :: m a -> m b -> m b
--   x >> y = x >>= \_ -> y
--
--   fail :: String -> m a
--   fail msg = error msg

-- instance Monad Maybe where
--    return x = Just x
--    Nothing >>= f = Nothing
--    Just x >>= f  = f x
--    fail _ = Nothing

-- ghci> return "WHAT" :: Maybe String
-- Just "WHAT"
-- ghci> Just 9 >>= \x -> return (x*10)
-- Just 90
-- ghci> Nothing >>= \x -> return (x*10)
-- Nothing


-- Walk the line

type Birds = Int
type Pole = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

-- If we make a function like this:
--    x -: f = f x
-- We can apply functions by first writing the parameter and then the function:
-- ghci> (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
-- (3,1)

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise                    = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise                    = Nothing

-- We need a way of taking a Maybe Pole and feeding it to a function
-- that takes a Pole and returns a Maybe Pole.
-- Luckily, we have >>=, which does just that for Maybe.
--
-- ghci> landRight 1 (0,0) >>= landLeft 2
-- Just (2,1)
-- Here's a sequence of birdy landings:
--   ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
--   Just (2,4)

banana :: Pole -> Maybe Pole
banana _ = Nothing
-- ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
-- Nothing
--
-- Instead of making functions that ignore their input and just return
-- a predetermined monadic value, we can use the >> function:
-- ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- Nothing


-- 'do' notation

-- 'do' notation isn't just for IO, but can be used for any monad.

-- a nested use of >>= :
-- ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
-- Just "3!"

-- Let's write this in a script and have each Maybe value take up its own line:
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))
-- To save us from writing all these annoying lambdas,
-- Haskell gives us do notation.
foo :: Maybe String
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- 1)
-- ghci> Just 9 >>= (\x -> Just (x > 8))
-- Just True
-- 2)
marySue :: Maybe Bool
marySue = do
    x <- Just 9
    Just (x > 8)
-- If we compare these two, it's easy to see why the result of the whole
-- monadic value is the result of the last monadic value in the do expression
-- with all the previous ones chained into it.

routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft' 2 start
    second <- landRight' 2 first
    landLeft' 1 second
-- Again, let's take a look at what this piece of code would look like
-- if we hadn't used the monadic aspects of Maybe:
routine' :: Maybe Pole
routine' =
    case Just (0,0) of
        Nothing -> Nothing
        Just start -> case landLeft 2 start of
            Nothing -> Nothing
            Just first -> case landRight 2 first of
                Nothing -> Nothing
                Just second -> landLeft 1 second

-- Here's an example of pattern matching in a do expression:
justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x
-- What if this pattern matching were to fail? When matching on a pattern
-- in a function fails, the next pattern is matched.
-- If the matching falls through all the patterns for a given function,
-- an error is thrown and our program crashes.

-- Here's a do expression with a pattern that's bound to fail:
wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x
-- ghci> wopwop
-- Nothing
-- The failed pattern matching has caused a failure within the context of our
-- monad instead of causing a program-wide failure,


-- The list monad

-- instance Monad [] where
--    return x = [x]
--    xs >>= f = concat (map f xs)
--    fail _ = []

-- ghci> [] >>= \x -> ["bad","mad","rad"]
-- []
-- ghci> [1,2,3] >>= \x -> []
-- []

-- When you have non-deterministic values interacting, you can view their
-- computation as a tree where every possible result in a list represents a
-- separate branch.
--
-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- Here's the previous expression rewritten in do notation:
listOfTuples :: [(Int,Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)
-- ghci> [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
-- In fact, list comprehensions are just syntactic sugar
-- for using lists as monads.

-- The MonadPlus type class is for monads that can also act as monoids:
-- class Monad m => MonadPlus m where
--    mzero :: m a
--    mplus :: m a -> m a -> m a
--
-- instance MonadPlus [] where
--    mzero = []
--    mplus = (++)

-- The guard function is defined like this:
-- guard :: (MonadPlus m) => Bool -> m ()
-- guard True = return ()
-- guard False = mzero

-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]
-- Here's the previous example rewritten in 'do' notation:
sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

-- A knight's quest
-- Say you have a chess board and only one knight piece on it.
-- We want to find out if the knight can reach a certain position
-- in three moves.
--
type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

-- This function can also be written without the use of lists as a monad:
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c,r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- Now, let's make a function that takes two positions and tells us
-- if you can get from one to the other in exactly three steps:
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start


-- Monad laws

-- 1) Left identity
-- return x >>= f is the same damn thing as f x

-- 2) Right identity
-- m >>= return is no different than just m
-- When we feed monadic values to functions by using >>=, those functions
-- take normal values and return monadic ones. 'return' is also one such
-- function, if you consider its type.

-- Left identity and right identity are basically laws that describe
-- how 'return' should behave.

-- 3) Associativity
-- Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)

-- By using >>=, we can compose two monadic functions:
-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)
-- f <=< g = (\x -> g x >>= f)

-- So what does that have to do with the associativity law? Well, when we look
-- at the law as a law of compositions, it states that:
-- f <=< (g <=< h) should be the same as (f <=< g) <=< h
