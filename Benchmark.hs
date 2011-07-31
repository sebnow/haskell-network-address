module Main where
import Prelude hiding (fromInteger)
import Criterion.Main
import Data.IP

ipv4str :: Integral a => a -> String
ipv4str n = "192.168." ++ show (n `div` 255)
                       ++ "."
                       ++ show (n `mod` 255)


ipv4 :: IPv4
ipv4 = fromInteger 3232235777

ipv6 :: IPv6
ipv6 = fromInteger 42540766452641154071740215577757643572

main = defaultMain
    [ bgroup "IPv4"
        [ bgroup "Parse"
            [ bench "read" $ whnf (readAddress :: String -> IPv4) "192.168.1.1"
            ]
        , bgroup "PrettyPrint"
            [ bench "show" $ whnf showAddress ipv4
            ]
        ]
    , bgroup "IPv6"
        [ bgroup "Parse"
            [ bench "read" $ whnf (readAddress :: String -> IPv6) "2001:0db8:85a3:0:0:8a2e:0370:7334"
            ]
        , bgroup "PrettyPrint"
            [ bench "show" $ whnf showAddress ipv6
            ]
        ]
    ]

