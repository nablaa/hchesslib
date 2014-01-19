import Test.HUnit

import System.Exit (exitFailure, exitSuccess)

exampleTest :: Test
exampleTest = TestList [
          1 ~=? 2 - 1
        , 2 ~=? 3 - 1
        ]

tests :: Test
tests = exampleTest

main :: IO ()
main = do c <- runTestTT $ tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
