{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Data.IP (
    Address(..),
    Subnet(..),
    IPv4(..),
    IPv6(..),
    IPv4Subnet,
    Mask,
    showIPv4Subnet,
    readIPv4Subnet,
) where

import Control.Monad (when)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.List (foldl', intercalate)
import Data.Word
import Numeric (showHex, readHex)
import Text.Printf (printf)

-- |An IP network mask
type Mask = Integer

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded, Show, Read)

-- |The abstract data structure to represent an IPv6 address.
data IPv6 = IPv6 !Word64 !Word64
            deriving (Eq, Ord, Show, Read)

-- |The abstract data structure to represent an IPv4 subnetwork.
data IPv4Subnet = IPv4Subnet IPv4 Mask deriving (Eq, Ord)

class (Eq a) => Address a where
    -- |Convert the byte representation to an IP 'Address'.
    fromInteger :: Integer -> a
    -- |Return the byte representation of an IP 'Address'.
    toInteger :: a -> Integer
    -- |Parse a textual representation of an IP 'Address'.
    readAddress :: String -> a
    -- |Return a conanical textual representation of an IP 'Address'.
    showAddress :: a -> String

-- |The 'Subnet' class is used to perform operations on and manipulate
-- IP subnetworks.
class (Address a) => Subnet s a | s -> a where
    -- |Return the first 'IP' in the 'Subnet'.
    base   :: s -> a
    -- |Return an 'Integer' representation of the subnet mask.
    mask :: s -> Mask
    -- |Determine if an 'IP' is within the range of a 'Subnet'.
    member :: a -> s -> Bool

--
-- IPv4
--

instance Address IPv4 where
    toInteger = fromIntegral . fromIPv4
    fromInteger = toIPv4 . fromIntegral
    readAddress = readIPv4
    showAddress = showIPv4

-- |Return the byte representation of an IPv4 IP address.
toIPv4 :: Word32 -> IPv4
toIPv4 = IPv4

-- |Convert the byte representation to an IPv4 address.
fromIPv4 :: IPv4 -> Word32
fromIPv4 (IPv4 x) = x

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
    toInteger = fromIPv6
    fromInteger = toIPv6
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

instance Show IPv4Subnet where
    show (IPv4Subnet ip mask) = "readIPv4Subnet \"" ++ showIPv4 ip ++ "/" ++ (show . maskToInt) mask ++ "\""

instance Read IPv4Subnet where
    readsPrec _ s = case take 14 s of
        "readIPv4Subnet" -> [(readIPv4Subnet . init . tail . dropWhile (/= '"') $ s, "")]
        otherwise        -> [(undefined, s)]

instance Subnet IPv4Subnet IPv4 where
    base (IPv4Subnet b _) = b
    mask (IPv4Subnet _ m) = m
    member ip (IPv4Subnet b m) = fromIPv4 ip .&. fromIntegral m == fromIPv4 b

-- |Create an 'IPv4Subnet' data structure.
ipv4Subnet :: IPv4 -> Mask -> IPv4Subnet
ipv4Subnet ip m = IPv4Subnet (ipv4Base ip m) m

-- |Return the base 'IPv4' address given an address and a network mask.
ipv4Base :: IPv4 -> Mask -> IPv4
ipv4Base (IPv4 ip) mask = IPv4 . flip shiftL n . flip shiftR n $ ip
    where n = maskToInt mask

-- |Return a conanical textual representation of an IPv4 IP address and
-- subnet.
showIPv4Subnet :: IPv4Subnet -> String
showIPv4Subnet (IPv4Subnet ip mask) = showIPv4 ip ++ "/" ++ (show . maskToInt) mask

-- |Parse a textual representation of an IPv4 IP address and subnet.
readIPv4Subnet :: String -> IPv4Subnet
readIPv4Subnet s = ipv4Subnet ip m
    where (ip, s') = readIPv4' s
          -- TODO: Support mask notation (192.168.1.1/255.255.0.0)
          m        = intToMask . fromIntegral . digitsToInt . drop 1 $ s'

-- |Convert a decimal mask to binary (e.g. 8 -> 1111.0000.0000.0000).
intToMask :: Int -> Mask
intToMask size | size < 32 = foldl setBit 0 . reverse $ [32 - size .. 31]
              | otherwise = 0

-- |Convert a binary mask to decimal (e.g. 1111.0000.0000.0000 -> 8).
maskToInt :: Mask -> Int
maskToInt m = case filter (testBit m) $ [0 .. 31] of
    (x:_) -> 32 - x
    []    -> 0

-- |Parse an integer.
digitsToInt :: String -> Int
digitsToInt = foldl' ((+) . (10 *)) 0 . map digitToInt

