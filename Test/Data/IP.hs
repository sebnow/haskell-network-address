module Test.Data.IP (tests) where
import Data.IP
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.List (intercalate)
import Data.Word

instance Arbitrary IPv4 where
    arbitrary = fmap (read . intercalate "." . map show) bytes
        where bytes = vectorOf 4 (arbitrary :: Gen Word8)

tests = [ testGroup "IPv4"
            [ testGroup "Read/Show"
                [ testProperty "Symmetric Read/Show" prop_ipv4_symmetric_readable
                ]
            , testGroup "Binary"
                [ testProperty "Symmetric to/from" prop_ipv4_symmetric_tofrom
                ]
            ]
        ]

prop_ipv4_symmetric_readable :: IPv4 -> Bool
prop_ipv4_symmetric_readable ip = (read . show) ip == id ip

prop_ipv4_symmetric_tofrom :: IPv4 -> Bool
prop_ipv4_symmetric_tofrom ip = (toIPv4 . fromIPv4) ip == id ip

