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
    IPAddress(..),
    readAddress,
    readSubnet,
    ipSubnet
) where

import Control.Monad (when)
import Data.Bits
import Data.Bits.Extras (trailingZeros)
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', findIndex, isSuffixOf, scanl)
import Data.Maybe (fromJust)
import Data.Word
import Numeric (showHex)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)
import Text.Read.Lex (readDecP, readHexP)

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded, Show, Read)

-- |The abstract data structure to represent an IPv6 address.
data IPv6 = IPv6 !Word64 !Word64
            deriving (Eq, Ord, Show, Read)

-- |The abstract data structure to represent an IP subnetwork.
data (IPAddress a) => IPSubnet a = IPSubnet a a
            deriving (Eq, Ord, Show, Read)

class (Eq a) => Address a where
    -- |Convert the byte representation to an IP 'Address'.
    toAddress   :: Integer -> a
    -- |Return the byte representation of an IP 'Address'.
    fromAddress :: a -> Integer
    -- |Parse a textual representation of an IP 'Address'.
    readsAddress :: ReadS a
    -- |Return a conanical textual representation of an IP 'Address'.
    showAddress :: a -> String

class (Address a) => IPAddress a where
--    ipAddrAny        :: a
--    ipAddrComplement :: a -> a
--    ipAddrShift      :: Int -> a
    ipAddrMask       :: a -> a -> a
    prefixFromLen    :: Int -> a
    lenFromPrefix    :: a -> Int

-- |The 'Subnet' class is used to perform operations on and manipulate
-- IP subnetworks.
class (IPAddress a) => Subnet s a | s -> a where
    -- |Parse a textual representation of a 'Subnet'.
    readsSubnet :: ReadS s
    -- |Return a conanical textual representation of a 'Subnet'.
    showSubnet :: s -> String
    -- |Return the first 'IP' in the 'Subnet'.
    base       :: s -> a
    -- |Return an binary representation of the subnet mask.
    netmask    :: s -> a
    -- |Determine if an 'IP' is within the range of a 'Subnet'.
    member     :: a -> s -> Bool
    member ip s = ip `ipAddrMask` netmask s == base s


instance IPAddress IPv4 where
    ipAddrMask (IPv4 a) (IPv4 b) = IPv4 (a .&. b)
    prefixFromLen l = IPv4 (complement 0 `shift` (32 - l))
    lenFromPrefix (IPv4 0) = 0
    lenFromPrefix (IPv4 a) = fromIntegral $ 32 - trailingZeros a

instance IPAddress IPv6 where
    ipAddrMask (IPv6 a1 a2) (IPv6 b1 b2) = IPv6 (a1 .&. b1) (a2 .&. b2)
    prefixFromLen l
        | l <= 64   = IPv6 (complement 0 `shift` (64 - l)) 0
        | otherwise = IPv6 (complement 0) (complement 0 `shift` (128 - l))
    lenFromPrefix (IPv6 0 0) = 0
    lenFromPrefix (IPv6 a 0) = fromIntegral $ 64 - trailingZeros a
    lenFromPrefix (IPv6 _ a) = fromIntegral $ 128 - trailingZeros a

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

-- |Parse a textual representation of an 'IPv6' IP address,
-- e.g. @1080:0:0:0:8:800:200C:417A@
readsIPv6 :: ReadS IPv6
readsIPv6 = readP_to_S readpIPv6

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

-- |Create an 'IPSubnet' data structure.
ipSubnet :: (IPAddress a, Integral b) => a -> b -> IPSubnet a
ipSubnet ip m = IPSubnet (ip `ipAddrMask` mask) mask
    where
        mask = prefixFromLen $ fromIntegral m

-- |Return a conanical textual representation of an IPv4 'Address' and
-- 'Subnet'.
showsIPv4Subnet :: IPSubnet IPv4 -> ShowS
showsIPv4Subnet (IPSubnet ip m) = showsIPv4 ip . showChar '/' . shows n
    where n = lenFromPrefix m

-- |Parse a textual representation of an IPv4 address and subnet.
readsIPv4Subnet :: ReadS (IPSubnet IPv4)
readsIPv4Subnet = readP_to_S readpIPv4Subnet

-- |Return an IPv4 subnet parser.
readpIPv4Subnet :: ReadP (IPSubnet IPv4)
readpIPv4Subnet = do
    ip <- readpIPv4
    _ <- char '/'
    m <- readDecP :: ReadP Word32
    if 0 <= m && m <= 32 then return (ipSubnet ip $ fromIntegral m) else pfail

-- |Return a conanical textual representation of an IPv6 'Address' and
-- 'Subnet'.
showsIPv6Subnet :: IPSubnet IPv6 -> ShowS
showsIPv6Subnet (IPSubnet ip m) = showsIPv6 ip . showChar '/' . shows n
    where n = lenFromPrefix m

-- |Parse a textual representation of an IPv6 address and subnet.
readsIPv6Subnet :: ReadS (IPSubnet IPv6)
readsIPv6Subnet = readP_to_S readpIPv6Subnet

-- |Return an IPv6 subnet parser.
readpIPv6Subnet :: ReadP (IPSubnet IPv6)
readpIPv6Subnet = do
    ip <- readpIPv6
    _ <- char '/'
    m <- readDecP :: ReadP Int
    if 0 <= m && m <= 128 then return (ipSubnet ip $ fromIntegral m) else pfail
