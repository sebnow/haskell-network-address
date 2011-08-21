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
    toMask,
    fromMask,
) where

import Control.Monad (when)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', intercalate)
import Data.Word
import Numeric (showHex, readHex)
import Text.Printf (printf)

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
    readAddress :: String -> a
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
    readSubnet :: String -> s
    -- |Return a conanical textual representation of a 'Subnet'.
    showSubnet :: s -> String
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

instance Address IPv4 where
    fromAddress (IPv4 x) = toInteger x
    toAddress   = IPv4 . fromInteger
    readAddress = readIPv4
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

-- |Parse a textual representation of an 'IPv4' IP address, returning the
-- address and the remainded of the 'String'.
readIPv4' :: String -> (IPv4, String)
readIPv4' s = (IPv4 $ sum . zipWith shift ds $ [24, 16, 8, 0], s')
    where (a, _:xs1)     = span isDigit s
          (b, _:xs2)     = span isDigit xs1
          (c, _:xs3)     = span isDigit xs2
          (d, s')        = span isDigit xs3
          ds             = map (fromIntegral . digitsToInt) [a, b, c, d] :: [Word32]

-- |Parse a textual representation of an 'IPv4' IP address,
-- e.g. @127.0.0.1@
readIPv4 :: String -> IPv4
readIPv4 = fst . readIPv4'

--
-- IPv6
--

instance Address IPv6 where
    fromAddress = fromIPv6
    toAddress   = toIPv6
    readAddress = readIPv6
    showAddress = showIPv6

-- |Parse a textual representation of an 'IPv6' IP address,
-- e.g. @1080:0:0:0:8:800:200C:417A@
readIPv6 :: String -> IPv6
-- TODO: Support collapsed 0's and alternative syntax, as per RFC3513.
readIPv6 s = case ipv6octets s of
    Just os   -> toIPv6 . octetsToInteger . map (fst . head . readHex) $ os
    otherwise -> error "Data.IP: no parse"

octetsToInteger :: [Word16] -> Integer
octetsToInteger = foldl' (\x y -> (x `shift` 16) + fromIntegral y) 0

-- |Return a conanical textual representation of an 'IPv6' IP address,
-- e.g. @fedc:ba98:7654:3210:fedc:ba98:7654:3210@
showIPv6 :: IPv6 -> String
-- TODO: Collapse the longest group of 0's, as per RFC5952.
showIPv6 = intercalate ":" . map (`showHex` "") . reverse . octets . fromIPv6
    where octets x = case x `divMod` (2 ^ 16) of
            (0 , o) -> [o]
            (x', o) -> o : octets x'

-- |Split an IPv6 into octets (i.e. by ':').
ipv6octets :: String -> Maybe [String]
ipv6octets s = case span (/= ':') s of
    ("", ':':os) -> fmap ((:) "0") (ipv6octets os)
    ("", "")     -> Just ["0"]
    (o, ':':os)  -> fmap ((:) o) (ipv6octets os)
    (o, "")      -> Just [o]
    otherwise    -> Nothing

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
    readSubnet = readIPSubnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

instance Subnet (IPSubnet IPv6) IPv6 where
    showSubnet = showIPSubnet
    readSubnet = readIPSubnet
    base (IPSubnet b _) = b
    netmask (IPSubnet _ m) = m

-- |Create an 'IPv4Subnet' data structure.
ipSubnet :: (Address a) => a -> Mask -> IPSubnet a
ipSubnet ip m = IPSubnet (maskAddress ip m) m

-- |Return a conanical textual representation of an IP 'Address' and
-- 'Subnet'.
showIPSubnet :: (Address a) => IPSubnet a -> String
showIPSubnet (IPSubnet ip mask) = showAddress ip ++ "/" ++ show (fromMask mask :: Integer)

-- |Parse a textual representation of an IP address and subnet.
readIPSubnet :: (Address a) => String -> IPSubnet a
readIPSubnet s = ipSubnet ip mask
    where (x, '/':y) = span (/= '/') s
          mask       = toMask . digitsToInt $ y
          ip         = readAddress x

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

-- |Parse an integer.
digitsToInt :: String -> Int
digitsToInt = foldl' ((+) . (10 *)) 0 . map digitToInt

