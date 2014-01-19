import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import BoardTests
import MoveTests

tests :: Test
tests = TestList [boardTests, moveTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
