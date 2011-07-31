module Test.Data.IP (tests) where
import Data.Bits (shift)
import Data.List (intercalate)
import Data.Word
import Numeric (showHex)
import qualified Data.IP as IP
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary IP.IPv4 where
    arbitrary = fmap (IP.fromInteger . toInteger) (arbitrary :: Gen Word32)

instance Arbitrary IP.IPv6 where
    arbitrary = do
        a <- arbitrary :: Gen Word64
        b <- arbitrary :: Gen Word64
        return . IP.fromInteger . toInteger $ (a `shift` 64) + b

instance Arbitrary IP.IPv4Subnet where
    arbitrary = do
        ip <- arbitrary :: Gen IP.IPv4
        size <- choose (8, 32 :: Int)
        return . IP.readIPv4Subnet $ IP.showAddress ip ++ "/" ++ show size

tests = [ testGroup "IPv4"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                , testProperty "Symmetric readAddress/showAddress" prop_ipv4_symmetric_parsable
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv4_symmetric_tofrom
                ]
            , testGroup "Base"
                [ testProperty "Base Idempotent" prop_ipv4_base_id
                ]
            ]
        , testGroup "IPv6"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                , testProperty "Symmetric readAddress/showAddress" prop_ipv6_symmetric_parsable
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv6_symmetric_tofrom
                ]
            ]
        , testGroup "Subnet"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric IPv4 Read/Show" prop_subnet_ipv4_symmetric_readable
                ]
            ]
        ]

prop_fun_id :: (Eq a) => (a -> a) -> a -> Bool
prop_fun_id f x = f x == f (f x)

prop_ipv4_symmetric_readable :: IP.IPv4 -> Bool
prop_ipv4_symmetric_readable ip = (read . show) ip == id ip

prop_ipv4_symmetric_parsable :: IP.IPv4 -> Bool
prop_ipv4_symmetric_parsable ip = (IP.readAddress . IP.showAddress) ip == id ip

prop_ipv4_symmetric_tofrom :: IP.IPv4 -> Bool
prop_ipv4_symmetric_tofrom ip = (IP.fromInteger . IP.toInteger) ip == id ip

prop_ipv4_base_id :: IP.IPv4 -> IP.Mask -> Bool
prop_ipv4_base_id ip mask = prop_fun_id (flip IP.ipv4Base mask) ip

prop_subnet_ipv4_symmetric_readable :: IP.IPv4Subnet -> Bool
prop_subnet_ipv4_symmetric_readable subnet = (read . show) subnet == id subnet

prop_ipv6_symmetric_readable :: IP.IPv6 -> Bool
prop_ipv6_symmetric_readable ip = (read . show) ip == id ip

prop_ipv6_symmetric_parsable :: IP.IPv6 -> Bool
prop_ipv6_symmetric_parsable ip = (IP.readAddress . IP.showAddress) ip == id ip

prop_ipv6_symmetric_tofrom :: IP.IPv6 -> Bool
prop_ipv6_symmetric_tofrom ip = (IP.fromInteger . IP.toInteger) ip == id ip

