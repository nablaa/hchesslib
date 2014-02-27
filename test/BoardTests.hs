module BoardTests where

import Piece
import Board
import Test.HUnit
import Data.Maybe
import TestUtils

boardPrintingTests :: Test
boardPrintingTests = TestList [
          "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact initialBoard
        , "        \n        \n        \n        \n        \n        \n        \n        \n" ~=? printBoardCompact emptyBoard
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
        , "a8" ~=? printCoordinate (0, 0)
        , "b8" ~=? printCoordinate (0, 1)
        , "c8" ~=? printCoordinate (0, 2)
        , "a7" ~=? printCoordinate (1, 0)
        , "a6" ~=? printCoordinate (2, 0)
        , "f5" ~=? printCoordinate (3, 5)
        ]

getPieceTest :: Test
getPieceTest = TestList [
          Nothing ~=? getPiece initialBoard (4, 4)
        , Nothing ~=? getPiece initialBoard (-1, -1)
        , Nothing ~=? getPiece initialBoard (8, 8)
        , Just (Piece Black Rook) ~=? getPiece initialBoard (0, 0)
        , Just (Piece Black Queen) ~=? getPiece initialBoard (0, 3)
        , Just (Piece White King) ~=? getPiece initialBoard (7, 4)
        ]

movePieceTest :: Test
movePieceTest = TestList [
          Nothing ~=? movePiece initialBoard (0, 0) (8, 8)
        , Nothing ~=? movePiece initialBoard (-1, -1) (7, 7)
        , Nothing ~=? movePiece initialBoard (4, 4) (0, 0)
        , " nbqkbnr\npppppppp\n        \n        \n    r   \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece initialBoard (0, 0) (4, 4)))
        , "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece initialBoard (0, 0) (0, 0)))
        , " rbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece initialBoard (0, 0) (0, 1)))
        ]

parsingBoardCompactTest :: Test
parsingBoardCompactTest = TestList [
          Just initialBoard ~=? parseBoardCompact (printBoardCompact initialBoard)
        , Just emptyBoard ~=? parseBoardCompact "        \n        \n        \n        \n        \n        \n        \n        \n"
        , Nothing ~=? parseBoardCompact "        \n        \n        \n        \n        \n        \n        \n        \n        \n"
        , Nothing ~=? parseBoardCompact "         \n        \n        \n        \n        \n        \n        \n        \n"
        , Nothing ~=? parseBoardCompact ""
        , Nothing ~=? parseBoardCompact "foobar"
        , Nothing ~=? parseBoardCompact "        \n  xxx   \n        \n        \n        \n        \n        \n        \n"
        , Nothing ~=? parseBoardCompact "rnbqkbnr\nppxppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"
        , Nothing ~=? parseBoardCompact "rnbqkbnr\npppppppp\n        \n\n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"
        , Nothing ~=? parseBoardCompact "pppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"
        ]

isEmptyTest :: Test
isEmptyTest = TestList [
          True ~=? isEmpty initialBoard (4, 4)
        , True ~=? isEmpty initialBoard (-1, -1)
        , True ~=? isEmpty initialBoard (8, 8)
        , False ~=? isEmpty initialBoard (0, 0)
        , False ~=? isEmpty initialBoard (0, 3)
        , False ~=? isEmpty initialBoard (7, 4)
        ]

isOpponentSquareTest :: Test
isOpponentSquareTest = TestList [
          False ~=? isOpponentSquare initialBoard (4, 4) White
        , False ~=? isOpponentSquare initialBoard (-1, -1) Black
        , False ~=? isOpponentSquare initialBoard (8, 8) White
        , False ~=? isOpponentSquare initialBoard (0, 0) Black
        , True ~=? isOpponentSquare initialBoard (0, 3) White
        , False ~=? isOpponentSquare initialBoard (7, 4) White
        , True ~=? isOpponentSquare initialBoard (7, 4) Black
        ]

firstPieceInSquareListTest :: Test
firstPieceInSquareListTest = TestList [
          Nothing ~=? firstPieceInSquareList initialBoard [(4, 1), (4, 2), (4, 3)]
        , Just (Piece White Pawn) ~=? firstPieceInSquareList initialBoard [(4, 1), (5, 2), (6, 3), (7, 4)]
        , Just (Piece White King) ~=? firstPieceInSquareList initialBoard [(7, 4), (0, 4)]
        ]

iterateDirectionInsideBoardTest :: Test
iterateDirectionInsideBoardTest = TestList [
          [(3,1),(2,1),(1,1),(0,1)] ~=? iterateDirectionInsideBoard (4, 1) (-1, 0)
        , [(5,2),(6,3),(7,4)] ~=? iterateDirectionInsideBoard (4, 1) (1, 1)
        , [] ~=? iterateDirectionInsideBoard (4, 0) (0, -1)
        ]

getKingSquareTest :: Test
getKingSquareTest = TestList [
          coord "e1" ~=? getKingSquare initialBoard White
        , coord "e8" ~=? getKingSquare initialBoard Black
        ]

boardTests :: Test
boardTests = TestList [boardPrintingTests, boardCoordinateTests,
                       getPieceTest, movePieceTest, parsingBoardCompactTest,
                       isEmptyTest, isOpponentSquareTest,
                       firstPieceInSquareListTest,
                       iterateDirectionInsideBoardTest,
                       getKingSquareTest]
