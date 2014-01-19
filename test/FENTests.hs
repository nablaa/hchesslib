module FENTests where

import Test.HUnit

writeFENTests :: Test
writeFENTests = TestList [
        ]

readFENTests :: Test
readFENTests = TestList [
        ]

fenTests :: Test
fenTests = TestList [readFENTests, writeFENTests]
