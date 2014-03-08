import Test.HUnit

import System.Exit (exitFailure, exitSuccess)

tests :: Test
tests = TestList []

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
