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
