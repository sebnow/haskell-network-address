module Main where
import Criterion.Main
import Data.IP

ipv4str :: Integral a => a -> String
ipv4str n = "192.168." ++ show (n `div` 255)
                       ++ "."
                       ++ show (n `mod` 255)

main = defaultMain
    [ bgroup "IPv4"
        [ bgroup "Parse"
            [ bench "Read" $ whnf (read :: String -> IPv4) "readIPv4 \"192.168.1.1\""
            , bench "read" $ whnf readIPv4 "192.168.1.1"
            ]
        , bgroup "PrettyPrint"
            [ bench "Show" $ whnf show (toIPv4 3232235777)
            , bench "show" $ whnf showIPv4 (toIPv4 3232235777)
            ]
        ]
    ]

