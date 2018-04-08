import Data.List
-- import Data.List (nub, sort)
-- import Data.List hiding (nub)
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- Data.List

-- foldl' and foldl1' are stricter versions of their respective lazy incarnations.
-- When using lazy folds on really big lists, you might often get a stack overflow error.
-- The culprit for that is that due to the lazy nature of the folds,
-- the accumulator value isn't actually updated as the folding happens.
-- What actually happens is that the accumulator kind of makes a promise that
-- it will compute its value when asked to actually produce the result
-- (also called a thunk). That happens for every intermediate accumulator and
-- all those thunks overflow your stack. The strict folds aren't lazy buggers
-- and actually compute the intermediate values as they go along instead of
-- filling up your stack with thunks. So if you ever get stack overflow errors
-- when doing lazy folds, try switching to their strict versions.

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
