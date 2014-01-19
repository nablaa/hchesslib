import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import Board

boardPrintingTests :: Test
boardPrintingTests = TestList [
        "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact initialBoard
        ]

tests :: Test
tests = boardPrintingTests

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
