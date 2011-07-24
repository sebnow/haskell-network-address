module Data.IP (
    IPv4,
    toIPv4,
    fromIPv4,
    showIPv4,
    readIPv4,
) where

import Control.Monad (when)
import Data.Bits (shift)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Char (digitToInt, isDigit)
import Data.List (foldl')
import Data.Word
import Text.Printf (printf)

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded)

instance Show IPv4 where
    show ip = "readIPv4 \"" ++ showIPv4 ip ++ "\""

instance Read IPv4 where
    readsPrec _ s = case take 8 s of
        "readIPv4" -> [(readIPv4 . init . tail . dropWhile (/= '"') $ s, "")]
        otherwise  -> [(undefined, s)]

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

-- |Parse a textual representation of an IPv4 IP address,
-- e.g. @127.0.0.1@
readIPv4 :: String -> IPv4
readIPv4 s = IPv4 . sum . zipWith shift ds $ [24, 16, 8, 0]
    where (a, _:xs1)     = span isDigit s
          (b, _:xs2)     = span isDigit xs1
          (c, _:xs3)     = span isDigit xs2
          (d, s')        = span isDigit xs3
          ds             = map digitsToWord32 [a, b, c, d]
          digitsToWord32 = foldl' ((+) . (10 *)) 0 . map (fromIntegral . digitToInt)

