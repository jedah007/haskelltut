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
