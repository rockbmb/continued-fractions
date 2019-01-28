#!/usr/bin/env runhaskell
module Main where

import Test.Framework (defaultMain)

import Math.CFTests (cfTests)

main :: IO ()
main = defaultMain cfTests