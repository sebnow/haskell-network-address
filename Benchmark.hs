module Main where
import Criterion.Main
import Data.IP

ipv4str :: Integral a => a -> String
ipv4str n = "192.168." ++ show (n `div` 255)
                       ++ "."
                       ++ show (n `mod` 255)

readIPv4 :: String -> IPv4
readIPv4 = read

main = defaultMain
    [ bgroup "IPv4"
        [ bench "Read" $ whnf readIPv4 (ipv4str 1)
        , bench "Show" $ whnf show (toIPv4 3232235777)
        ]
    ]

