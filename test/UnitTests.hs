import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import BoardTests
import MoveTests
import FENTests

tests :: Test
tests = TestList [boardTests, moveTests, fenTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
