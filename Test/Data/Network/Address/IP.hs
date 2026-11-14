{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Data.Network.Address.IP (tests) where
import Data.Bits (shift, shiftR, xor)
import Data.Char (isDigit, isHexDigit)
import Data.List (intercalate, isPrefixOf)
import Data.Network.Address.IP
import Data.Word
import Numeric (showHex)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

-- |Integral type for 32bit network masks (i.e. 0..32)
newtype Prefix32 = Prefix32 {getPrefix32 :: Word8} deriving (Show, Read)

newtype Prefix128 = Prefix128 {getPrefix128 :: Word8} deriving (Show, Read)

instance Arbitrary IPv4 where
    arbitrary = fmap (toAddress . toInteger) (arbitrary :: Gen Word32)

instance Arbitrary IPv6 where
    arbitrary = do
        a <- arbitrary :: Gen Word64
        b <- arbitrary :: Gen Word64
        return . toAddress . toInteger $ (a `shift` 64) + b

instance Arbitrary (IPSubnet IPv4) where
    arbitrary = do
        ip <- arbitrary :: Gen IPv4
        size <- arbitrary :: Gen Prefix32
        return . readSubnet $ showAddress ip ++ "/" ++ show (getPrefix32 size)

instance Arbitrary (IPSubnet IPv6) where
    arbitrary = do
        ip <- arbitrary :: Gen IPv6
        size <- arbitrary :: Gen Prefix128
        return . readSubnet $ showAddress ip ++ "/" ++ show (getPrefix128 size)

instance Arbitrary Prefix32 where
    arbitrary = fmap (Prefix32 . fromIntegral) (choose (0, 32) :: Gen Int)

instance Arbitrary Prefix128 where
    arbitrary = fmap (Prefix128 . fromIntegral) (choose (0, 128) :: Gen Int)

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
            , testGroup "Membership"
                [ testProperty "Reflexivity" prop_reflexivityIPv4
                , testProperty "Reflexivity for subnet" prop_reflexivityIPv4Subnet
                , testProperty "BitsyContains" prop_bitsyContainsIPv4
                , testProperty "Subnet below range" prop_subnetBelowRangeIPv4
                , testProperty "Subnet above range" prop_subnetAboveRangeIPv4
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
            , testGroup "Membership"
                [ testProperty "Reflexivity" prop_reflexivityIPv6
                , testProperty "Reflexivity for subnet" prop_reflexivityIPv6Subnet
                , testProperty "BitsyContains" prop_bitsyContainsIPv6
                , testProperty "Subnet below range" prop_subnetBelowRangeIPv6
                , testProperty "Subnet above range" prop_subnetAboveRangeIPv6
                ]
            ]
        ]

mostSignificantBit :: Integer -> Integer
mostSignificantBit n
    | n <= 0    = error "Non-positive integer provided"
    | otherwise = go 0 n
  where
    go !pos x
      | x == 1    = pos
      | otherwise = go (pos + 1) (x `shiftR` 1)

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

prop_subnet_ipv4_symmetric_readable :: IPSubnet IPv4 -> Bool
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

prop_subnet_ipv6_symmetric_readable :: IPSubnet IPv6 -> Bool
prop_subnet_ipv6_symmetric_readable subnet = (readSubnet . showSubnet) subnet == id subnet

prop_subnet_ipv6_invalid_reads :: IPSubnet IPv6 -> String -> Property
prop_subnet_ipv6_invalid_reads = prop_subnet_invalid_reads

prop_subnet_ipv4_invalid_reads :: IPSubnet IPv4 -> String -> Property
prop_subnet_ipv4_invalid_reads = prop_subnet_invalid_reads

prop_reflexivityIPv4 :: IPv4 -> IPSubnet IPv4 -> Property
prop_reflexivityIPv4 addr (IPSubnet _ p) = property $ member addr (ipSubnetForPrefix addr p)

prop_reflexivityIPv4Subnet :: IPSubnet IPv4 -> Property
prop_reflexivityIPv4Subnet s@(IPSubnet a _) = property $ member a s

prop_reflexivityIPv6 :: IPv6 -> IPSubnet IPv6 -> Property
prop_reflexivityIPv6 addr (IPSubnet _ p) = property $ member addr (ipSubnetForPrefix addr p)

prop_reflexivityIPv6Subnet :: IPSubnet IPv6 -> Property
prop_reflexivityIPv6Subnet s@(IPSubnet a _) = property $ member a s

prop_bitsyContainsIPv4 :: IPv4 ->  IPSubnet IPv4 -> Property
prop_bitsyContainsIPv4 a s@(IPSubnet sa p) =
    let xed = fromAddress a `xor` fromAddress sa
    in (xed == 0 || mostSignificantBit xed < maxPrefix a - p) === member a s

prop_bitsyContainsIPv6 :: IPv6 ->  IPSubnet IPv6 -> Property
prop_bitsyContainsIPv6 a s@(IPSubnet sa p) =
    let xed = fromAddress a `xor` fromAddress sa
    in (xed == 0 || mostSignificantBit xed < maxPrefix a - p) === member a s

prop_subnetBelowRangeIPv4 :: IPSubnet IPv4 -> Property
prop_subnetBelowRangeIPv4 s@(IPSubnet a _) =
    fromAddress a > 0
    ==> not (member (toAddress . subtract 1 . fromAddress $ a) s)

prop_subnetBelowRangeIPv6 :: IPSubnet IPv6 -> Property
prop_subnetBelowRangeIPv6 s@(IPSubnet a _) =
    fromAddress a > 0
    ==> not (member (toAddress . subtract 1 . fromAddress $ a) s)

prop_subnetAboveRangeIPv4 :: IPSubnet IPv4 -> Property
prop_subnetAboveRangeIPv4 = prop_subnetAboveRange

prop_subnetAboveRangeIPv6 :: IPSubnet IPv6 -> Property
prop_subnetAboveRangeIPv6 = prop_subnetAboveRange

-- |The first address after the subnet is not a member.
prop_subnetAboveRange :: Subnet (IPSubnet a) a => IPSubnet a -> Property
prop_subnetAboveRange s@(IPSubnet a p) =
    next < 2^(maxPrefix a) ==> not (member (toAddress next) s)
  where
    next = fromAddress a + 2^(maxPrefix a - p)
