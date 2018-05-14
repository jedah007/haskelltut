-- Functors redux

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b

-- Functor laws
-- 1)  If we map the id function over a functor, the functor that we get back
-- should be the same as the original functor:
-- fmap id = id
-- 2) Composing two functions and then mapping the resulting function
-- over a functor should be the same as first mapping one function over the functor
-- and then mapping the other one:
-- fmap (f . g) = fmap f . fmap g
-- bzw. fmap (f . g) F = fmap f (fmap g F)

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- ghci> fmap id (CJust 0 "haha")
-- CJust 1 "haha"
-- ghci> id (CJust 0 "haha")
-- CJust 0 "haha"
-- => does not obey the 1st functor law => no Functor


-- Applicative functors

-- So far, when we were mapping functions over functors,
-- we usually mapped functions that take only one parameter.
-- But what happens when we map a function like *,
-- which takes two parameters, over a functor?
-- Doing fmap (*) (Just 3) results in Just ((*) 3),
-- which can also be written as Just (* 3) if we use sections.
-- Interesting! We get a function wrapped in a Just!


-- But what if we have a functor value of Just (3 *) and a functor value of
-- Just 5 and we want to take out the function from Just (3 *) and map it over
-- Just 5? With normal functors, we're out of luck, because all they support
-- is just mapping normal functions over existing functors.

-- class (Functor f) => Applicative f where
--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b

-- A better way of thinking about pure would be to say that it takes a value
-- and puts it in some sort of default (or pure) context — a minimal context
-- that still yields that value.
-- <*> takes a functor that has a function in it and another functor and sort
-- of extracts that function from the first functor and then maps it over the
-- second one. When I say extract, I actually sort of mean run and then extract,
-- maybe even sequence.

-- instance Applicative Maybe where
--   pure = Just
--   Nothing <*> _ = Nothing
--   (Just f) <*> something = fmap f something
-- So for Maybe, <*> extracts the function from the left value if it's a Just
-- and maps it over the right value. If any of the parameters is Nothing,
-- Nothing is the result.

-- <*> is left-associative, which means that pure (+) <*> Just 3 <*> Just 5
-- is the same as (pure (+) <*> Just 3) <*> Just 5

-- pure f <*> x equals fmap f x
-- (<$>) :: (Functor f) => (a -> b) -> f a -> f b
-- f <$> x = fmap f x
-- By using <$>, the applicative style really shines, because now if we want
-- to apply a function f between three applicative functors, we can write
-- f <$> x <*> y <*> z.
-- If the parameters weren't applicative functors but normal values,
-- we'd write f x y z.
--
-- ghci> (++) <$> Just "johntra" <*> Just "volta"
-- Just "johntravolta"
-- ghci> (++) "johntra" "volta"
-- "johntravolta"

-- instance Applicative [] where
--    pure x = [x]
--   fs <*> xs = [f x | f <- fs, x <- xs]
--
-- ghci> [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]

-- instance Applicative IO where
--    pure = return
--    a <*> b = do
--       f <- a
--       x <- b
--       return (f x)

myAction :: IO String
myAction = do
    a <- getLine
    b <- getLine
    return $ a ++ b
-- same in applicative style:
myAction :: IO String
myAction = (++) <$> getLine <*> getLine

-- ZipList
-- instance Applicative ZipList where
--        pure x = ZipList (repeat x)
--        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
-- It applies the first function to the first value,
-- the second function to the second value, etc.
-- ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]
-- [101,102,103]

-- The (,,) function is the same as '\x y z -> (x,y,z)'.
-- Also, the (,) function is the same as '\x y -> (x,y)'.

-- liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
-- liftA2 f a b = f <$> a <*> b
-- 'liftA2' takes a normal binary function and promotes it to a function
-- that operates on two functors.
--
-- Let's say we have Just 3 and Just [4]. How do we get Just [3,4]?
-- ghci> liftA2 (:) (Just 3) (Just [4])
-- Just [3,4]
-- ghci> (:) <$> Just 3 <*> Just [4]
-- Just [3,4]

sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-- Applicatve Functor laws
-- 1) pure f <*> x = fmap f x
-- 2) pure id <*> v = v
-- 3) pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 4) pure f <*> pure x = pure (f x)
-- 5) u <*> pure y = pure ($ y) <*> u


-- On newtype laziness

--  The type keyword is for making type synonyms. What that means is that we
-- just give another name to an already existing type so that the type is
-- easier to refer to. Say we did the following:
--    type IntList = [Int]

-- The newtype keyword is for taking existing types and wrapping them
-- in new types, mostly so that it's easier to make them instances of certain
-- type classes. When we use newtype to wrap an existing type, the type that
-- we get is separate from the original type.
--    newtype CharList = CharList { getCharList :: [Char] }

-- The data keyword is for making your own data types and with them,
-- you can go hog wild. They can have as many constructors and fields as you
-- wish and can be used to implement any algebraic data type by yourself.

-- If you just want your type signatures to look cleaner and be more descriptive,
-- you probably want type synonyms. If you want to take an existing type
-- and wrap it in a new type in order to make it an instance of a type class,
-- chances are you're looking for a newtype. And if you want to make something
-- completely new, odds are good that you're looking for the data keyword.


-- Monoids

-- class Monoid m where
--    mempty :: m
--    mappend :: m -> m -> m
--    mconcat :: [m] -> m
--    mconcat = foldr mappend mempty
-- mempty represents the identity value for a particular monoid.
-- mappend, which, as you've probably guessed, is the binary function.
-- mconcat takes a list of monoid values and reduces them to a single value
-- by doing mappend between the list's elements.

-- Monoid laws
-- 1) mempty `mappend` x = x
-- 2) x `mappend` mempty = x
-- 3) (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Lists are monoids
-- instance Monoid [a] where
--    mempty = []
--    mappend = (++)
-- Notice that we wrote instance Monoid [a] and not instance Monoid [],
-- because Monoid requires a concrete type for an instance.

-- Product and Sum
-- newtype Product a =  Product { getProduct :: a }
--    deriving (Eq, Ord, Read, Show, Bounded)
--
-- instance Num a => Monoid (Product a) where
--    mempty = Product 1
--    Product x `mappend` Product y = Product (x * y)

-- Any and All
--
