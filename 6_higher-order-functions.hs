-- Curried functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

multWithEighteen :: (Num a) => a -> a
multWithEighteen = multTwoWithNine 2


compareHundredWith :: (Num a, Ord a) => a -> Ordering
compareHundredWith x = compare 100 x

compareHundredWith' :: (Num a, Ord a) => a -> Ordering
compareHundredWith' = compare 100


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])


-- SOme higher-orderism is in order
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Remember that when you're making functions, especially higher order ones,
-- and you're unsure of the type, you can just try omitting the type declaration
-- and then checking what Haskell infers it to be by using :t.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g where
  g x y = f y x

-- whoaa
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y
-- e.g.: zipWith (flip' div) [2,2..] [10,8,6,4,2]


-- Maps and filters
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter' (<=x) xs)
        biggerSorted = quicksort (filter' (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where
    p x = x `mod` 3829 == 0

squaresOddSum :: (Integral a) => a -> a
squaresOddSum n = sum (takeWhile (<n) (filter odd (map (^2) [1..])))

squaresOddSum' :: (Integral a) => a -> a
squaresOddSum' n = sum (takeWhile (<n) [x^2 | x <- [1..], odd (x^2)])


-- Collatz-sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where
    isLong xs = length xs > 15

listOfFuns :: (Num a, Enum a) => [(a -> a)]
listOfFuns = map (*) [0..]


-- Lambas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x


-- Only folds and horses
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (+) 0 xs -- better

-- Generally, if you have a function like 'foo a = bar b a',
-- you can rewrite it as 'foo = bar b', because of currying.

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- worse because '++' far more expensive
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- One big difference is that right folds work on infinite lists,
-- whereas left ones don't!

-- Folds can be used to implement any function where you traverse a list once,
-- element by element, and then return something based on that.
-- Whenever you want to traverse a list to return something,
-- chances are you want a fold.

-- Example library functions redefined

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- 'scanl' and 'scanr' are like 'foldl' and 'foldr', only they report all the
-- intermediate accumulator states in the form of a list.

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- We use 'takeWhile' here instead of 'filter' because filter doesn't
-- work on infinite lists


-- Function application with '$'
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- Consider the expression 'sum (map sqrt [1..130])'.
-- Because $ has such a low precedence, we can rewrite that expression as
-- 'sum $ map sqrt [1..130]'

-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- >> [7.0,30.0,9.0,1.7320508075688772]


-- Function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- The expression f (g (z x)) is equivalent to (f . g . z) x
