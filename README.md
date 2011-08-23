Network Address
===============

The network-address library provides data structures for IP addresses as
well as sub-networks. The goal is to have a generic, fast and complete
library for representing network addresses in binary or textual form.
Currently it supports IPv4 and IPv6 (partially).


Usage
=====

    *Data.Network.Address> readAddress "127.0.0.1" :: IPv4
    IPv4 2130706433
    *Data.Network.Address> let ip = readAddress "127.0.0.1" :: IPv4
    *Data.Network.Address> showAddress ip
    "127.0.0.1"
    *Data.Network.Address> let subnet = readSubnet "192.168.1.42/8" ::
    IPSubnet IPv4
    *Data.Network.Address> subnet
    IPSubnet (IPv4 3232235776) (-256)
    *Data.Network.Address> showSubnet subnet
    "192.168.1.0/8"
    *Data.Network.Address> showAddress . base $ subnet
    "192.168.1.0"
    *Data.Network.Address> (readAddress "192.168.1.5" :: IPv4) `member`
    subnet
    True
    *Data.Network.Address> (readAddress "192.168.2.5" :: IPv4) `member`
    subnet
    False


Installation
============

The network-address library is build using Cabal and Hackage. Just run:

    cabal configure
    cabal build
    cabal install


Stability
=========

The library has not reached v1.0.0 yet, and thus has an unstable API.
And minor version increase (e.g. v0.1.0 -> v0.2.0) may break backwards
compatibility until v1.0.0 is reached.


Documentation
=============

Documentation is available online on Hackage:
http://hackage.haskell.org/package/network-address

Documentation can be generated locally using Cabal and Haddock:

    cabal haddock


Testing
=======

The test suite can be built and run with Cabal:

    cabal configure --enable-tests
    cabal build
    cabal test


License
=======

The network-address library is licensed under the MIT license. Please read
the LICENSE file for details.

