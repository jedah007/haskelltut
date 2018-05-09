import qualified Data.Map as Map

-- Algebraic data types intro

-- We could also opt not to export any value constructors for Shape by just writing Shape in the export statement.
-- That way, someone importing our module could only make shapes by using the auxilliary functions baseCircle and baseRect.
-- Data.Map uses that approach. You can't create a map by doing Map.Map [(1,2),(3,4)] because it doesn't export that value constructor.
-- However, you can make a mapping by using one of the auxilliary functions like Map.fromList.
-- Remember, value constructors are just functions that take the fields as parameters and return a value of some type (like Shape) as a result.
-- So when we choose not to export them, we just prevent the person importing our module from using those functions,
-- but if some other functions that are exported return a type, we can use them to make values of our custom data types.

-- Not exporting the value constructors of a data types makes them more abstract in such a way that we hide their implementation.
-- Also, whoever uses our module can't pattern match against the value constructors.
module Shapes
    ( Point(..)
    , Shape(..) -- Kurzform für: Shape(Circle, Rectangle)
    , surface
    , nudge
    , baseCircle
    , baseRectangle
    ) where

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

-- Record syntax

-- The main benefit of this is that it creates functions that lookup fields in the data type.
-- By using record syntax to create this data type, Haskell automatically made
-- these functions: firstName, lastName, age, height, phoneNumber and flavor.
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber ::  String
                     , flavor ::  String
                     } deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
-- Erzeugung: Car {company="Ford", model="Mustang", year=1967}
-- Reihenfolge egal bei dieser Syntax

-- Type parameters

-- e.g. data Maybe a = Nothing | Just a
-- The a here is the type parameter. And because there's a type parameter involved,
-- we call Maybe a type constructor. Depending on what we want this data type to hold when it's not Nothing,
-- this type constructor can end up producing a type of Maybe Int, Maybe Car, Maybe String, etc.
-- No value can have a type of just Maybe, because that's not a type per se, it's a type constructor.
-- In order for this to be a real type that a value can be part of, it has to have all its type parameters filled up.

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- However, it's a very strong convention in Haskell to never add typeclass constraints in data declarations.
-- Why? Well, because we don't benefit a lot, but we end up writing more class constraints,
-- even when we don't need them

data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

-- Derived instances

-- When we derive the Eq instance for a type and then try to compare two values of that type with == or /=,
-- Haskell will see if the value constructors match (there's only one value constructor here though) and
-- then it will check if all the data contained inside matches by testing each pair of fields with ==.
-- There's only one catch though, the types of all the fields also have to be part of the Eq typeclass.
data Person2 = Person2 {
fName :: String,
lName :: String
} deriving (Eq, Show, Read)


-- read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person

-- We can also read parameterized types, but we have to fill in the type parameters.
-- So we can't do read "Just 't'" :: Maybe a, but we can do read "Just 't'" :: Maybe Char.

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Because all the value constructors are nullary (take no parameters, i.e. fields),
-- we can make it part of the Enum typeclass.
-- The Enum typeclass is for things that have predecessors and successors.
-- We can also make it part of the Bounded typeclass, which is for things that have a
-- lowest possible value and highest possible value.

-- Type synonyms

-- e.g. type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- Type synonyms can also be parameterized
type AssocList k v = [(k,v)]

-- Just like we can partially apply functions to get new functions,
-- we can partially apply type parameters and get new type constructors from them
type IntMap v = Map Int v

-- Another cool data type that takes two types as its parameters is the Either a b type
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- Recursive data structures

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- When we define functions as operators, we can use that to give them a fixity (but we don't have to).
-- A fixity states how tightly the operator binds and whether it's left-associative or right-associative.
-- For instance, *'s fixity is infixl 7 * and +'s fixity is infixl 6.
-- That means that they're both left-associative (4 * 3 * 2 is (4 * 3) * 2) but * binds tighter than +,
-- because it has a greater fixity, so 5 * 4 + 3 is (5 * 4) + 3
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node (treeInsert x left) right
  | x > a  = Node left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right


-- Typeclasses 102

-- class Eq a where
--    (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- So 'class' is for defining new typeclasses and
-- 'instance' is for making our types instances of typeclasses.
-- Because == was defined in terms of /= and vice versa in the class declaration,
-- we only had to overwrite one of them in the instance declaration.

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- You can also make typeclasses that are subclasses of other typeclasses.
-- class (Eq a) => Num a where
--    ...

-- with type constructors:
-- wrong:
-- instance Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False
-- right:
-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

-- If you want to see what the instances of a typeclass are,
-- just do ':info YourTypeClass' in GHCI


-- A yes-no typeclass

class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
instance YesNo [a] where
  yesno [] = False
  yesno _ = True
instance YesNo Bool where
  yesno = id
-- What's id? It's just a standard library function that takes a parameter and
-- returns the same thing, which is what we would be writing here anyway.
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False
instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True
instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


-- The Functor typeclass

-- ,which is basically for things that can be mapped over

-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b
-- Now, the 'f' is not a concrete type (a type that a value can hold, like Int,
-- Bool or Maybe String), but a type constructor that takes one type parameter.

-- The type signature of map is, it's: map :: (a -> b) -> [a] -> [b]
-- instance Functor [] where
--    fmap = map
-- [] is a type constructor that takes one type and can produce types
-- such as [Int], [String] or even [[String]]

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


-- Kinds and some type-foo
