-- Bytestrings come in two flavors: strict and lazy ones.
-- Strict bytestrings reside in Data.ByteString and they do away with the laziness completely.
-- There are no promises involved; a strict bytestring represents a series of bytes in an array.
-- You can't have things like infinite strict bytestrings.
-- If you evaluate the first byte of a strict bytestring, you have to evaluate it whole.
-- The upside is that there's less overhead because there are no thunks (the technical term for promise) involved.
-- The downside is that they're likely to fill your memory up faster because they're read into memory at once.

-- The other variety of bytestrings resides in Data.ByteString.Lazy.
-- They're lazy, but not quite as lazy as lists. Like we said before,
-- there are as many thunks in a list as there are elements.
-- That's what makes them kind of slow for some purposes.
-- Lazy bytestrings take a different approach — they are stored in chunks (not to be confused with thunks!),
-- each chunk has a size of 64K. So if you evaluate a byte in a lazy bytestring (by printing it or something),
-- the first 64K will be evaluated. After that, it's just a promise for the rest of the chunks.
-- Lazy bytestrings are kind of like lists of strict bytestrings with a size of 64K.
-- When you process a file with lazy bytestrings, it will be read chunk by chunk.
-- This is cool because it won't cause the memory usage to skyrocket and the 64K probably fits neatly into your CPU's L2 cache.
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S


-- pack
-- pack :: [Word8] -> ByteString
-- Takes a list, which is lazy, and making it less lazy,
-- so that it's lazy only at 64K intervals.

-- Word8: like Int, only that it has a much smaller range, namely 0-255.

-- ghci> B.pack [99,97,110]
-- Chunk "can" Empty

-- Empty is like the [] for lists.


-- unpack
-- inverse function of pack


-- fromChunks
-- takes a list of strict bytestrings and converts it to a lazy bytestring.
-- toChunks
-- takes a lazy bytestring and converts it to a list of strict ones.


-- The bytestring version of ':' is called 'cons'

-- ...


-- Whenever you need better performance in a program that reads a lot of data into strings,
-- give bytestrings a try, chances are you'll get some good performance boosts
-- with very little effort on your part.
-- I usually write programs by using normal strings and then convert them
-- to use bytestrings if the performance is not satisfactory.
