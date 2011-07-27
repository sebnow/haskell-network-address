module Test.Data.IP (tests) where
import Data.List (intercalate)
import Data.Word
import Data.IP
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

instance Arbitrary IPv4 where
    arbitrary = fmap (readIPv4 . intercalate "." . map show) bytes
        where bytes = vectorOf 4 (arbitrary :: Gen Word8)

instance Arbitrary IPv4Subnet where
    arbitrary = do
        ip <- arbitrary
        size <- choose (8, 32 :: Int)
        return . readIPv4Subnet $ showIPv4 ip ++ "/" ++ show size

tests = [ testGroup "IPv4"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv4_symmetric_tofrom
                ]
            , testGroup "Base"
                [ testProperty "Base Idempotent" prop_ipv4_base_id
                ]
            ]
        , testGroup "Subnet"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric IPv4 Read/Show" prop_subnet_ipv4_symmetric_readable
                ]
            ]
        ]

prop_ipv4_symmetric_readable :: IPv4 -> Bool
prop_ipv4_symmetric_readable ip = (read . show) ip == id ip

prop_ipv4_symmetric_tofrom :: IPv4 -> Bool
prop_ipv4_symmetric_tofrom ip = (toIPv4 . fromIPv4) ip == id ip

prop_ipv4_base_id :: IPv4 -> Mask -> Bool
prop_ipv4_base_id ip mask = ipv4Base ip mask == ipv4Base (ipv4Base ip mask) mask

prop_subnet_ipv4_symmetric_readable :: IPv4Subnet -> Bool
prop_subnet_ipv4_symmetric_readable subnet = (read . show) subnet == id subnet

