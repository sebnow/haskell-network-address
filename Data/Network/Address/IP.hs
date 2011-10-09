{-
Copyright (c) 2011 Sebastian Nowicki <sebnow@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
-}

{- |Module     : Data.Network.Address.IP
    License    : MIT
    Stability  : experimental
    Portability: unportable
-}

{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}
module Data.Network.Address.IP (
    Address(..),
    Subnet(..),
    IPv4(..),
    IPv6(..),
    IPSubnet(..),
    Mask,
    fromMask,
    toMask,
) where

import Control.Monad (when)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', findIndex, isSuffixOf, scanl)
import Data.Maybe (fromJust)
import Data.Word
import Numeric (showHex)
import Text.ParserCombinators.ReadP
import Text.Read.Lex (readDecP, readHexP)

-- |The byte representation of an IP network mask.
type Mask = Integer

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded)

-- |The abstract data structure to represent an IPv6 address.
data IPv6 = IPv6 !Word64 !Word64
            deriving (Eq, Ord)

-- |The abstract data structure to represent an IP subnetwork.
data (Address a) => IPSubnet a = IPSubnet a Mask deriving (Eq, Ord)

class (Eq a, Read a, Show a) => Address a where
    -- |Convert the byte representation to an IP 'Address'.
    toAddress   :: Integer -> a
    -- |Return the byte representation of an IP 'Address'.
    fromAddress :: a -> Integer
    -- |Apply a mask to an 'Address'.
    maskAddress :: (Bits b, Integral b) => a -> b -> a
    maskAddress ip mask = toAddress . (\x -> x `div` (2^n) * 2^n) . fromAddress $ ip
        where n = fromMask mask

-- |The 'Subnet' class is used to perform operations on and manipulate
-- IP subnetworks.
class (Read s, Show s, Address a) => Subnet s a | s -> a where
    -- |Return the first 'IP' in the 'Subnet'.
    base       :: s -> a
    -- |Return an binary representation of the subnet mask.
    netmask    :: s -> Mask
    -- |Determine if an 'IP' is within the range of a 'Subnet'.
    member     :: a -> s -> Bool
    member ip s = fromAddress ip .&. netmask s == fromAddress (base s)

--
-- IPv4
--

instance Show IPv4 where
    showsPrec _ = showsIPv4

instance Read IPv4 where
    readsPrec _ = readP_to_S readpIPv4

instance Address IPv4 where
    fromAddress (IPv4 x) = toInteger x
    toAddress   = IPv4 . fromInteger

-- |Return a conanical textual representation of an IPv4 IP address,
-- e.g. @127.0.0.1@
showsIPv4 :: IPv4 -> ShowS
showsIPv4 (IPv4 ip) = shows a
     . showChar '.' . shows b
     . showChar '.' . shows c
     . showChar '.' . shows d
    where ( _, a) = shift8 r1
          (r1, b) = shift8 r2
          (r2, c) = shift8 r3
          (r3, d) = shift8 ip
          shift8  = (`divMod` 256)

-- |An IPv4 parser.
readpIPv4 :: ReadP IPv4
readpIPv4 = do
    a <- ipv4Digit
    (b:c:d:_) <- count 3 (char '.' >> ipv4Digit)
    return $ IPv4 (a `shift` 24 + b `shift` 16 + c `shift` 8 + d)

-- |Parse an 8 bit IPv4 digit
ipv4Digit :: ReadP Word32
ipv4Digit = do
    d <- many1 (satisfy isDigit)
    let x = fromIntegral . digitsToInt $ d
    if 0 <= x && x < 256 then return x else pfail

-- |Parse an unsigned integer.
digitsToInt :: String -> Int
digitsToInt = foldl' ((+) . (10 *)) 0 . map digitToInt

--
-- IPv6
--

instance Show IPv6 where
    showsPrec _ = showsIPv6

instance Read IPv6 where
    readsPrec _ = readP_to_S readpIPv6

instance Address IPv6 where
    fromAddress = fromIPv6
    toAddress   = toIPv6

-- |An IPv6 parser.
readpIPv6 :: ReadP IPv6
readpIPv6 = fmap (toAddress . word16sToInteger) $ choice
    [ readHexP >>= \o -> count 7 (char ':' >> readHexP) >>= \os -> return (o:os)
    , do
       head <- many1 (readHexP <<* char ':')
       tail <- many1 (char ':' >> readHexP)
       let body = replicate (8 - (length head + length tail)) 0
           os   = head ++ body ++ tail
       case length os of
           8 -> return os
           _ -> pfail
    , char ':' >> many1 (char ':' >> readHexP) >>= \xs ->
        if   length xs <= 8
        then return xs
        else pfail
    , string "::" >> return [0]
    , many1 (readHexP <<* char ':') <<* char ':' >>= \xs ->
        if   length xs > 8
        then pfail
        else return (xs ++ replicate (8 - length xs) 0)
    ]

infixl 4 <<*
-- |Monad version of 'Applicative's '<*' operator.
(<<*) :: Monad m => m a -> m b -> m a
a <<* b = a >>= (b >>) . return

word16sToInteger :: [Word16] -> Integer
word16sToInteger = foldl' (\x y -> (x `shift` 16) + fromIntegral y) 0

-- |Return a conanical textual representation of an 'IPv6' IP address,
-- e.g. @fedc:ba98:7654:3210:fedc:ba98:7654:3210@
showsIPv6 :: IPv6 -> ShowS
showsIPv6 ip | fromAddress ip == 0 = showString "::"
             | otherwise           = case indexOfLongestSequence 0 word16s of
    Nothing  -> foldl' (\s x -> s . showChar ':' . showHex x) (showHex field) fields
    Just idx -> if   idx == 0
                then fst3 $ foldl' (collapse idx) (showString "", 0, False) word16s
                else fst3 (foldl' (collapse idx) (showHex field, 1, False) fields) .
                    if   replicate (length word16s - idx) 0 `isSuffixOf` word16s
                    then showChar ':'
                    else id
    where word16s@(field:fields) = toIPv6Octets $ fromAddress ip
          fst3 (x, _, _) = x
          collapse idx (s, i, z) x | z && x == 0 = (s,                            i+1, True)
                                   | i == idx    = (s . showChar ':',             i+1, True)
                                   | otherwise   = (s . showChar ':' . showHex x, i+1, False)

-- |Find the longest, consecutive, occurance of an element within
-- a list.
indexOfLongestSequence :: (Eq a) => a -> [a] -> Maybe Int
indexOfLongestSequence x xs = case m of
    0 -> Nothing
    _ -> fmap (\x -> x - m + 1) $ findIndex (== m) ms
    where m  = maximum ms
          ms = drop 1 $ scanl (\c y -> if y == x then c + 1 else 0) 0 xs

-- |Split a number into 'IPv6' word16s.
toIPv6Octets :: Integral a => a -> [a]
toIPv6Octets = fill . reverse . go
    where fill os = replicate (8 - length os) 0 ++ os
          go x    = case x `divMod` (2 ^ 16) of
            (0, o) -> [o]
            (x', o) -> o : go x'

-- |Convert the byte representation to an 'IPv6' address.
toIPv6 :: Integer -> IPv6
toIPv6 x = IPv6 (fromIntegral a) (fromIntegral b)
    where ( _, a) = shift64 r1
          (r1, b) = shift64 x
          shift64 x = divMod (fromIntegral x) (2 ^ 64)

-- |Return the byte representation of an 'IPv6' IP address.
fromIPv6 :: IPv6 -> Integer
fromIPv6 (IPv6 a b) = (a' `shift` 64) + b'
    where a' = fromIntegral a
          b' = fromIntegral b

--
-- Subnet
--

instance Show (IPSubnet IPv4) where
    showsPrec _ = showsIPSubnet

instance Read (IPSubnet IPv4) where
    readsPrec _ = readP_to_S readpIPv4Subnet

instance Subnet (IPSubnet IPv4) IPv4 where
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

instance Show (IPSubnet IPv6) where
    showsPrec _ = showsIPSubnet

instance Read (IPSubnet IPv6) where
    readsPrec _ = readP_to_S readpIPv6Subnet

instance Subnet (IPSubnet IPv6) IPv6 where
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

-- |Create an 'IPv4Subnet' data structure.
ipSubnet :: (Address a) => a -> Mask -> IPSubnet a
ipSubnet ip m = IPSubnet (maskAddress ip m) m

-- |Return a conanical textual representation of an IP 'Address' and
-- 'Subnet'.
showsIPSubnet :: (Address a, Show a) => IPSubnet a -> ShowS
showsIPSubnet (IPSubnet ip mask) = shows ip . showChar '/' . shows n
    where n = (fromMask mask :: Integer)

-- |Return an IPv4 subnet parser.
readpIPv4Subnet :: ReadP (IPSubnet IPv4)
readpIPv4Subnet = do
    ip <- readpIPv4
    _ <- char '/'
    m <- readDecP :: ReadP Word32
    if 0 <= m && m <= 32 then return (ipSubnet ip (toMask m)) else pfail

-- |Return an IPv6 subnet parser.
readpIPv6Subnet :: ReadP (IPSubnet IPv6)
readpIPv6Subnet = do
    ip <- readpIPv6
    _ <- char '/'
    m <- readDecP :: ReadP Int
    if 0 <= m && m <= 128 then return (ipSubnet ip (toMask m)) else pfail

-- |Convert a decimal mask to binary (e.g. 8 -> 1111.0000.0000.0000).
toMask :: (Integral a, Bits a, Integral b, Bits b) => a -> b
toMask size | size > 0  = complement (2^size - 1)
            | otherwise = complement 0

-- |Convert a binary mask to decimal (e.g. 1111.0000.0000.0000 -> 8).
fromMask :: (Integral a, Bits a, Integral b) => a -> b
fromMask m = timesDivisible (fromIntegral . complement $ m) 2

-- |Determine how many times a number is divisible by another number.
timesDivisible :: (Integral a) => a -> a -> a
timesDivisible a x = timesDivisible' a x 0
    where timesDivisible' a x c | a > 0     = timesDivisible' (a `div` x) x (c + 1)
                                | otherwise = c

