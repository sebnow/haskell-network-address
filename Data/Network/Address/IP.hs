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

import Control.Monad (when)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', intercalate, findIndex, groupBy)
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

-- |The abstract data structure to represent an IPv4 subnetwork.
data (Address a) => IPSubnet a = IPSubnet a Mask deriving (Eq, Ord, Show, Read)

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
readSubnet :: (Address a, Subnet s a) => String -> s
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
    showAddress = showIPv4

-- |Return a conanical textual representation of an IPv4 IP address,
-- e.g. @127.0.0.1@
showIPv4 :: IPv4 -> String
showIPv4 (IPv4 ip) = printf "%d.%d.%d.%d" a b c d
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
    showAddress = showIPv6

-- |Parse a textual representation of an 'IPv6' IP address,
-- e.g. @1080:0:0:0:8:800:200C:417A@
readsIPv6 :: ReadS IPv6
readsIPv6 = readP_to_S readpIPv6

-- |An IPv6 parser.
readpIPv6 :: ReadP IPv6
readpIPv6 = fmap (toAddress . octetsToInteger) $ choice
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

octetsToInteger :: [Word16] -> Integer
octetsToInteger = foldl' (\x y -> (x `shift` 16) + fromIntegral y) 0

-- |Return a conanical textual representation of an 'IPv6' IP address,
-- e.g. @fedc:ba98:7654:3210:fedc:ba98:7654:3210@
showIPv6 :: IPv6 -> String
showIPv6 ip = intercalate ":" fields
    where fields  = if   m == 1
                    then map (`showHex` "") octets
                    else (init ++ [""] ++ tail)
          init    = if   i == 0
                    then [""]
                    else map (`showHex` "") . concat . take i $ grouped
          tail    = if   i == (length lengths - 1)
                    then [""]
                    else map (`showHex` "") . concat . drop (i + 1) $ grouped
          i       = fromJust $ findIndex (== m) lengths
          m       = maximum lengths
          lengths = map length grouped
          grouped = groupBy (\x y -> x == 0 && y == 0) octets
          octets  = toIPv6Octets . fromAddress $ ip

-- |Split a number into 'IPv6' octets.
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

instance Subnet (IPSubnet IPv4) IPv4 where
    showSubnet = showIPSubnet
    readsSubnet = readsIPv4Subnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

instance Subnet (IPSubnet IPv6) IPv6 where
    showSubnet = showIPSubnet
    readsSubnet = readsIPv6Subnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

-- |Create an 'IPv4Subnet' data structure.
ipSubnet :: (Address a) => a -> Mask -> IPSubnet a
ipSubnet ip m = IPSubnet (maskAddress ip m) m

-- |Return a conanical textual representation of an IP 'Address' and
-- 'Subnet'.
showIPSubnet :: (Address a) => IPSubnet a -> String
showIPSubnet (IPSubnet ip mask) = showAddress ip ++ "/" ++ show (fromMask mask :: Integer)

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

