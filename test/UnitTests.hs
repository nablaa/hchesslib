import Test.HUnit

import System.Exit (exitFailure, exitSuccess)
import Board

boardPrintingTests :: Test
boardPrintingTests = TestList [
        "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact initialBoard
        ]

boardCoordinateTests :: Test
boardCoordinateTests = TestList [
          True ~=? isInsideBoard (0, 0)
        , True ~=? isInsideBoard (7, 7)
        , True ~=? isInsideBoard (3, 5)
        , False ~=? isInsideBoard (-1, 5)
        , False ~=? isInsideBoard (3, -1)
        , False ~=? isInsideBoard (8, 5)
        , False ~=? isInsideBoard (3, 8)
        , Just (0, 0) ~=? parseCoordinate "a8"
        , Just (0, 1) ~=? parseCoordinate "b8"
        , Just (0, 2) ~=? parseCoordinate "c8"
        , Just (1, 0) ~=? parseCoordinate "a7"
        , Just (2, 0) ~=? parseCoordinate "a6"
        , Just (7, 0) ~=? parseCoordinate "a1"
        , Just (0, 7) ~=? parseCoordinate "h8"
        , Just (7, 7) ~=? parseCoordinate "h1"
        , Just (3, 5) ~=? parseCoordinate "f5"
        , Nothing ~=? parseCoordinate "B5"
        , Nothing ~=? parseCoordinate "F5"
        , Nothing ~=? parseCoordinate "foobar"
        , Nothing ~=? parseCoordinate "12"
        , Nothing ~=? parseCoordinate "i1"
        , Nothing ~=? parseCoordinate "a9"
        , Nothing ~=? parseCoordinate "a-1"
        ]

tests :: Test
tests = TestList [boardPrintingTests, boardCoordinateTests]

main :: IO ()
main = do c <- runTestTT tests
          if failures c > 0 || errors c > 0 then exitFailure
                                            else exitSuccess
