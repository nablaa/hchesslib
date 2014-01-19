import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import BoardTests
import MoveTests
import FENTests
import PieceTests

tests :: Test
tests = TestList [boardTests, moveTests, fenTests, pieceTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
