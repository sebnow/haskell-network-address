{-# LANGUAGE FlexibleInstances #-}
module Test.Data.Network.Address.IP (tests) where
import Data.Bits (shift)
import Data.Char (isDigit, isHexDigit)
import Data.LargeWord
import Data.List (intercalate, isPrefixOf)
import Data.Network.Address.IP
import Data.Word
import Numeric (showHex)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- |Integral type for 32bit network masks (i.e. 0..32)
newtype Mask32 = Mask32 {getMask32 :: Word8} deriving (Show, Read)

newtype Mask128 = Mask128 {getMask128 :: Word8} deriving (Show, Read)

instance Arbitrary IPv4 where
    arbitrary = fmap (toAddress . toInteger) (arbitrary :: Gen Word32)

instance Arbitrary IPv6 where
    arbitrary = do
        a <- arbitrary :: Gen Word64
        b <- arbitrary :: Gen Word64
        return . toAddress . toInteger $ (a `shift` 64) + b

instance Arbitrary (IPSubnet IPv4 Word32) where
    arbitrary = do
        ip <- arbitrary :: Gen IPv4
        size <- arbitrary :: Gen Mask32
        return . readSubnet $ showAddress ip ++ "/" ++ show (getMask32 size)

instance Arbitrary (IPSubnet IPv6 Word128) where
    arbitrary = do
        ip <- arbitrary :: Gen IPv6
        size <- arbitrary :: Gen Mask128
        return . readSubnet $ showAddress ip ++ "/" ++ show (getMask128 size)

instance Arbitrary Mask32 where
    arbitrary = fmap (Mask32 . fromIntegral) (choose (0, 32) :: Gen Int)

instance Arbitrary Mask128 where
    arbitrary = fmap (Mask128 . fromIntegral) (choose (0, 128) :: Gen Int)

tests = [ testGroup "IPv4"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                , testProperty "Symmetric readAddress/showAddress" prop_ipv4_symmetric_parsable
                , testProperty "Invalid readsAddress" prop_ipv4_invalid_reads
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv4_symmetric_tofrom
                ]
            , testGroup "Subnet"
                [ testProperty "Symmetric IPv4 Read/Show" prop_subnet_ipv4_symmetric_readable
                ]
            ]
        , testGroup "IPv6"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                , testProperty "Symmetric readAddress/showAddress" prop_ipv6_symmetric_parsable
                , testProperty "Invalid readsAddress" prop_ipv6_invalid_reads
                , testProperty "Parse zero-compressed" prop_ipv6_parse_compressed
                , testProperty "Zero-compressed once" prop_ipv6_compressed_once
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv6_symmetric_tofrom
                ]
            , testGroup "Subnet"
                [ testProperty "Symmetric IPv6 Read/Show" prop_subnet_ipv6_symmetric_readable
                ]
            ]
        , testGroup "Netmask"
            [ testProperty "Symmetric toMask/fromMask" prop_mask_tofrom
            ]
        ]

readAddressMaybe :: Address a => String -> Maybe a
readAddressMaybe s = case [x | (x, "") <- readsAddress s] of
    [ip] -> Just ip
    _    -> Nothing

-- |Returns the indices of all sublists.
elemsIndices :: Eq a => [a] -> [a] -> [Int]
elemsIndices needle xs = reverse $ go needle xs 0 []
    where go needle [] i is = is
          go needle xs i is = case needle `isPrefixOf` xs of
            True  -> go needle (drop (length needle) xs) (i + 1) (i:is)
            False -> go needle (drop 1 xs) (i + 1) is

prop_fun_id :: (Eq a) => (a -> a) -> a -> Bool
prop_fun_id f x = f x == f (f x)

prop_ipv4_symmetric_readable :: IPv4 -> Bool
prop_ipv4_symmetric_readable ip = (read . show) ip == id ip

prop_ipv4_symmetric_parsable :: IPv4 -> Bool
prop_ipv4_symmetric_parsable ip = (readAddressMaybe . showAddress) ip == Just ip

prop_ipv4_symmetric_tofrom :: IPv4 -> Bool
prop_ipv4_symmetric_tofrom ip = (toAddress . fromAddress) ip == id ip

prop_ipv4_invalid_reads :: IPv4 -> String -> Property
prop_ipv4_invalid_reads a x = length x > 0 && (not . isDigit . head $ x)
    ==> (a, x) `elem` (readsAddress $ (showAddress a) ++ x)

prop_subnet_ipv4_symmetric_readable :: IPSubnet IPv4 Word32 -> Bool
prop_subnet_ipv4_symmetric_readable subnet = (readSubnet . showSubnet) subnet == id subnet

prop_ipv6_symmetric_readable :: IPv6 -> Bool
prop_ipv6_symmetric_readable ip = (read . show) ip == id ip

prop_ipv6_symmetric_parsable :: IPv6 -> Bool
prop_ipv6_symmetric_parsable ip = (readAddressMaybe . showAddress) ip == Just ip

prop_ipv6_symmetric_tofrom :: IPv6 -> Bool
prop_ipv6_symmetric_tofrom ip = (toAddress . fromAddress) ip == id ip

prop_ipv6_parse_compressed :: Word16 -> Word16 -> Word8 -> Bool
prop_ipv6_parse_compressed x y p = readAddressMaybe ip' == Just ip
    where h = fromIntegral x
          t = fromIntegral y
          ip = toAddress ((h `shift` 112) + t) :: IPv6
          os = showHex h "" : replicate (fromIntegral p `rem` 6) "0" ++ [showHex t ""]
          ip' = if   length os == 8
                then intercalate ":" os
                else intercalate ":" $ (head os ++ ":") : tail os

prop_ipv6_compressed_once :: IPv6 -> Bool
prop_ipv6_compressed_once ip = length ("::" `elemsIndices` (showAddress ip)) == 1

prop_ipv6_invalid_reads :: IPv6 -> String -> Property
prop_ipv6_invalid_reads a x = length x > 0 && (not . isHexDigit . head $ x)
    ==> (a, x) `elem` (readsAddress $ (showAddress a) ++ x)

prop_subnet_invalid_reads :: (Address a, Subnet s a, Eq s) => s -> String -> Property
prop_subnet_invalid_reads s x = length x > 0 && (not . isDigit . head $ x)
    ==> (s, x) `elem` (readsSubnet $ (showSubnet s) ++ x)

prop_subnet_ipv6_symmetric_readable :: IPSubnet IPv6 Word128 -> Bool
prop_subnet_ipv6_symmetric_readable subnet = (readSubnet . showSubnet) subnet == id subnet

prop_subnet_ipv6_invalid_reads :: IPSubnet IPv6 Word128 -> String -> Property
prop_subnet_ipv6_invalid_reads = prop_subnet_invalid_reads

prop_subnet_ipv4_invalid_reads :: IPSubnet IPv4 Word32 -> String -> Property
prop_subnet_ipv4_invalid_reads = prop_subnet_invalid_reads

prop_mask_tofrom :: Mask32 -> Bool
prop_mask_tofrom x = (fromMask m :: Word32) == (fromIntegral . getMask32) x
    where m = (toMask . getMask32) x :: Word32


