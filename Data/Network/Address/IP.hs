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
    readAddress,
    readSubnet,
    toMask,
) where

import Control.Monad (liftM2)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', findIndex, isSuffixOf, scanl)
import Data.Maybe (fromJust)
import Data.Word
import Numeric (showHex)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)
import Text.Read.Lex (readDecP, readHexP)

-- |The byte representation of an IP network mask.
type Mask = Integer

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded, Show, Read)

-- |The abstract data structure to represent an IPv6 address.
data IPv6 = IPv6 !Word64 !Word64
            deriving (Eq, Ord, Show, Read)

-- |The abstract data structure to represent an IP subnetwork.
data IPSubnet a = IPSubnet a Mask deriving (Eq, Ord, Show, Read)

class (Eq a) => Address a where
    -- |Convert the byte representation to an IP 'Address'.
    toAddress   :: Integer -> a
    -- |Return the byte representation of an IP 'Address'.
    fromAddress :: a -> Integer
    -- |Parse a textual representation of an IP 'Address'.
    readsAddress :: ReadS a
    -- |Return a conanical textual representation of an IP 'Address'.
    showAddress :: a -> String
    -- |Apply a mask to an 'Address'.
    maskAddress :: (Bits b, Integral b) => a -> b -> a
    maskAddress ip mask = toAddress . (\x -> x `div` (2^n) * 2^n) . fromAddress $ ip
        where n = fromMask mask

-- |The 'Subnet' class is used to perform operations on and manipulate
-- IP subnetworks.
class (Address a) => Subnet s a | s -> a where
    -- |Parse a textual representation of a 'Subnet'.
    readsSubnet :: ReadS s
    -- |Return a conanical textual representation of a 'Subnet'.
    showSubnet :: s -> String
    -- |Return the first 'IP' in the 'Subnet'.
    base       :: s -> a
    -- |Return an binary representation of the subnet mask.
    netmask    :: s -> Mask
    -- |Determine if an 'IP' is within the range of a 'Subnet'.
    member     :: a -> s -> Bool
    member ip s = fromAddress ip .&. netmask s == fromAddress (base s)

-- |@readAddress s@ parses an 'Address'. The 'String' @s@ must be
-- completely consumed.
readAddress :: (Address a) => String -> a
readAddress s = case [x | (x, "") <- readsAddress s] of
    [ip] -> ip
    []   -> error "readAddress: no parse"
    _    -> error "readAddress: ambiguous parse"

-- |@readSubnet s@ parses a 'Subnet'. The 'String' @s@ must be
-- completely consumed.
readSubnet :: (Subnet s a) => String -> s
readSubnet s = case [x | (x, "") <- readsSubnet s] of
    [ip] -> ip
    []   -> error "readSubnet: no parse"
    _    -> error "readSubnet: ambiguous parse"

--
-- IPv4
--

instance Address IPv4 where
    fromAddress (IPv4 x) = toInteger x
    toAddress   = IPv4 . fromInteger
    readsAddress = readsIPv4
    showAddress ip = showsIPv4 ip ""

instance Address Word32 where
    fromAddress = toInteger
    toAddress = fromInteger
    readsAddress = map (\(IPv4 x, s) -> (x, s)) . readsIPv4
    showAddress ip = showsIPv4 (IPv4 ip) ""

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

-- |Parse an 'IPv4' IP address.
readsIPv4 :: ReadS IPv4
readsIPv4 = readP_to_S readpIPv4

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

instance Address IPv6 where
    fromAddress = fromIPv6
    toAddress   = toIPv6
    readsAddress = readsIPv6
    showAddress ip = showsIPv6 ip ""

instance Address (Word32, Word32, Word32, Word32) where
    fromAddress = fromHostAddress6
    toAddress = toHostAddress6
    readsAddress = map (\(ip, s) -> ((toHostAddress6 . fromAddress) ip, s)) . readsIPv6
    showAddress = showAddress . toIPv6 . fromAddress

-- |Parse a textual representation of an 'IPv6' IP address,
-- e.g. @1080:0:0:0:8:800:200C:417A@
readsIPv6 :: ReadS IPv6
readsIPv6 = readP_to_S readpIPv6

-- |An IPv6 parser.
readpIPv6 :: ReadP IPv6
readpIPv6 = fmap (toAddress . word16sToInteger) $ choice
    [ liftM2 (:) readHexP (count 7 (char ':' >> readHexP))
    , do
       a <- upTo1 7 (readHexP <<* char ':')
       c <- choice [ upTo1 (8 - length a) (char ':' >> readHexP)
                   , char ':' >> return []
                   ]
       let b = replicate (8 - (length a + length c)) 0
       return $ a ++ b ++ c
    , char ':' >> upTo1 7 (char ':' >> readHexP)
    , string "::" >> return [0]
    ]

-- |@upTo n p@ parses zero or up to @n@ occurrances of @p@.
upTo :: Int -> ReadP a -> ReadP [a]
upTo n p | n <= 0    = return []
         | otherwise = return [] +++ upTo1 n p

-- |@upTo n p@ parses one or up to @n@ occurrances of @p@.
upTo1 :: Int -> ReadP a -> ReadP [a]
upTo1 n p = liftM2 (:) p (upTo (n - 1) p)

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

-- |Convert the byte representation to an 'HostAddress6' IP address.
toHostAddress6 :: Integer -> (Word32, Word32, Word32, Word32)
toHostAddress6 x = (a, b, c, d)
    where ( _, a) = shift32 r1
          (r1, b) = shift32 r2
          (r2, c) = shift32 r3
          (r3, d) = shift32 x
          shift32 x = divMod (fromIntegral x) (2 ^ 32)

-- |Return the byte representation of a 'HostAddress6' IP address.
fromHostAddress6 :: (Word32, Word32, Word32, Word32) -> Integer
fromHostAddress6 (a, b, c, d) = (a' `shift` 96) + (b' `shift` 64) + (c' `shift` 32) + d'
    where a' = fromIntegral a
          b' = fromIntegral b
          c' = fromIntegral c
          d' = fromIntegral d

--
-- Subnet
--

instance Subnet (IPSubnet IPv4) IPv4 where
    showSubnet s = showsIPv4Subnet s ""
    readsSubnet = readsIPv4Subnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

instance Subnet (IPSubnet IPv6) IPv6 where
    showSubnet s = showsIPv6Subnet s ""
    readsSubnet = readsIPv6Subnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

-- |Create an 'IPv4Subnet' data structure.
ipSubnet :: (Address a) => a -> Mask -> IPSubnet a
ipSubnet ip m = IPSubnet (maskAddress ip m) m

-- |Return a conanical textual representation of an IPv4 'Address' and
-- 'Subnet'.
showsIPv4Subnet :: IPSubnet IPv4 -> ShowS
showsIPv4Subnet (IPSubnet ip m) = showsIPv4 ip . showChar '/' . shows n
    where n = fromMask m :: Integer

-- |Parse a textual representation of an IPv4 address and subnet.
readsIPv4Subnet :: ReadS (IPSubnet IPv4)
readsIPv4Subnet = readP_to_S readpIPv4Subnet

-- |Return an IPv4 subnet parser.
readpIPv4Subnet :: ReadP (IPSubnet IPv4)
readpIPv4Subnet = do
    ip <- readpIPv4
    _ <- char '/'
    m <- readDecP :: ReadP Word32
    if 0 <= m && m <= 32 then return (ipSubnet ip (toMask m)) else pfail

-- |Return a conanical textual representation of an IPv6 'Address' and
-- 'Subnet'.
showsIPv6Subnet :: IPSubnet IPv6 -> ShowS
showsIPv6Subnet (IPSubnet ip m) = showsIPv6 ip . showChar '/' . shows n
    where n = fromMask m :: Integer

-- |Parse a textual representation of an IPv6 address and subnet.
readsIPv6Subnet :: ReadS (IPSubnet IPv6)
readsIPv6Subnet = readP_to_S readpIPv6Subnet

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

