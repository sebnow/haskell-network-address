module Main where
import Test.Framework (defaultMain)
import qualified Test.Data.Network.Address.IP as IP

main = defaultMain $ concat
    [ IP.tests
    ]

