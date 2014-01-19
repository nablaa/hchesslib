import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import BoardTests

tests :: Test
tests = TestList [boardTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
