module Data.IP (
    IPv4,
    toIPv4,
    fromIPv4,
) where

import Control.Monad (when)
import Data.Bits (shift)
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Word
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import Text.Parsec.Prim (unexpected)
import Text.Printf (printf)

-- |The abstract data structure to represent an IPv4 address.
data IPv4 = IPv4 !Word32
            deriving (Eq, Ord, Bounded)

instance Show IPv4 where
    show = showIPv4

instance Read IPv4 where
    readsPrec _ s = case P.parse (toReadS ipv4parser) "ipv4" s of
        Left _  -> error "ipv4"
        Right x -> x 

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
          shift8 = (`divMod` 256)

digitsToInt :: [Char] -> Int
digitsToInt = foldl' (\x y -> x * 10 + y) 0 . map digitToInt

ipv4digit :: P.Parser Int
ipv4digit = fmap digitsToInt (P.many1 P.digit)
--ipv4digit = do a <- [char '2', 
--               b <- oneOf ['1' ... '5']
--               c <- oneOf ['1' ... '5']
--               return digitsToInt [a, b, c]
--        <|> do a <- char '1'
--               b <- digit
--               c <- digit
--               return digitsToInt [a, b, c]
--        <|> fmap digitsToInt . count 2 digit
--        <|> fmap digitToInt digit

ipv4parser :: P.Parser IPv4
ipv4parser = do
    ds <- ipv4digit `P.sepBy1` (P.char '.')
    when (length ds /= 4) (unexpected "IPv4")
    mapM_ (\x -> when (x < 0 || x > 255) (unexpected "IPv4")) ds
    let [a, b, c, d] = ds
    return $ toIPv4' (a, b, c, d)

toIPv4' :: (Int, Int, Int, Int) -> IPv4
toIPv4' (a, b, c, d) = IPv4 . fromIntegral $ shift a 24 + shift b 16 + shift c 8 + d 

toReadS :: P.Parser a -> P.Parser [(a, String)]
toReadS p = do
    x <- p
    r <- P.getInput
    return [(x, r)]

