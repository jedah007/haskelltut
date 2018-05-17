import Data.Monoid

-- Writer? I hardly know her!

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)

-- ghci> (3, "Smallish gang.") `applyLog` isBigGang
-- (False,"Smallish gang.Compared gang size to 9")

-- Monoids to the rescue

-- Right now, applyLog takes values of type (a,String), but is there a reason
-- that the log has to be a String? It uses ++ to append the logs,
-- so wouldn't this work on any kind of list, not just a list of characters?
-- applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])

-- Would this work for bytestrings? There's no reason it shouldn't.
-- However, the type we have now only works for lists. It seems like we'd
-- have to make a separate applyLog for bytestrings. But wait! Both lists and
-- bytestrings are monoids. As such, they are both instances of the
-- Monoid type class.
applyLog' :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog' (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- Because the accompanying value can now be any monoid value, we no longer
-- have to think of the tuple as a value and a log, but now we can think of
-- it as a value with an accompanying monoid value.

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- ghci> ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})

-- The Writer type

-- newtype Writer w a = Writer { runWriter :: (a, w) }
-- It's wrapped in a newtype so that it can be made an instance of Monad and
-- that its type is separate from a normal tuple. The a type parameter
-- represents the type of the value and the w type parameter the type of the
-- attached monoid value.

-- instance (Monoid w) => Monad (Writer w) where
--    return x = Writer (x, mempty)
--    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Using 'do' notation with Writer
