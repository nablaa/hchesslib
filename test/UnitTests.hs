import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import MoveTests
import PieceTests

tests :: Test
tests = TestList [moveTests, pieceTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
