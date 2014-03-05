module BoardTests where

import Piece
import Board
import Test.HUnit
import Data.Maybe
import TestUtils
import qualified Data.Set as S

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
          Nothing ~=? movePiece (0, 0) (8, 8) initialBoard
        , Nothing ~=? movePiece (-1, -1) (7, 7) initialBoard
        , Nothing ~=? movePiece (4, 4) (0, 0) initialBoard
        , " nbqkbnr\npppppppp\n        \n        \n    r   \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece (0, 0) (4, 4) initialBoard))
        , "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece (0, 0) (0, 0) initialBoard))
        , " rbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" ~=? printBoardCompact (fromJust (movePiece (0, 0) (0, 1) initialBoard))
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

isSquareThreatenedTest :: Test
isSquareThreatenedTest = TestList [
          False ~=? isSquareThreatened initialBoard Black (coord "e1")
        , False ~=? isSquareThreatened initialBoard Black (coord "e4")
        , True ~=? isSquareThreatened initialBoard White (coord "e1")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "c6")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "h5")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g2")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d8")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "e5")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "f1")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "h1")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g5")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g8")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d5")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "b4")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g4")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "f4")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "b1")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "f4")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "d8")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d8")
        , True ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "f1")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "h6")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "b5")
        , False ~=? isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "c4")
        ]

isCheckTest :: Test
isCheckTest = TestList [
          False ~=? isCheck initialBoard White
        , False ~=? isCheck initialBoard Black
        , True ~=? isCheck (fenBoard "rnbqkbnr/pppp2pp/5p2/4p2Q/4P3/3P4/PPP2PPP/RNB1KBNR b KQkq - 1 3") Black
        , False ~=? isCheck (fenBoard "rnbqkbnr/pppp2pp/5p2/4p2Q/4P3/3P4/PPP2PPP/RNB1KBNR b KQkq - 1 3") White
        , True ~=? isCheck (fenBoard "rnbqk2r/pppp2bp/3N2pn/4pp1Q/4P3/3P4/PPP2PPP/RNB1KB1R b KQkq - 1 7") Black
        , True ~=? isCheck (fenBoard "rnbq3r/pppp2bp/3Nk1pn/3Ppp1Q/4P3/8/PPP2PPP/RNB1KB1R b KQ - 0 9") Black
        , False ~=? isCheck (fenBoard "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") Black
        , True ~=? isCheck (fenBoard "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") White
        , True ~=? isCheck (fenBoard "r3k2r/ppp2p1p/2n1p1p1/8/2B2P1q/2NPb1n1/PP4PP/R2Q3K w kq - 0 8") White
        ]

getSquaresWithOwnerTest :: Test
getSquaresWithOwnerTest = TestList [
          S.fromList [coord "a7", coord "g4"] ~=? S.fromList (getSquaresWithOwner (fenBoard "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1") Black)
        , S.fromList [coord "g2", coord "h2", coord "h3"] ~=? S.fromList (getSquaresWithOwner (fenBoard "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1") White)
        ]

getPlayerPiecesTest :: Test
getPlayerPiecesTest = TestList [
          S.fromList [Bishop, Bishop, Queen, King] ~=? S.fromList (getPlayerPieces (fenBoard "4k3/8/5np1/8/8/2BB4/2Q5/4K3 w - - 0 1") White)
        , S.fromList [Knight, Pawn, King] ~=? S.fromList (getPlayerPieces (fenBoard "4k3/8/5np1/8/8/2BB4/2Q5/4K3 w - - 0 1") Black)
        ]

getSquareColorTest :: Test
getSquareColorTest = TestList [
          Black ~=? getSquareColor (coord "a1")
        , Black ~=? getSquareColor (coord "c1")
        , Black ~=? getSquareColor (coord "g5")
        , Black ~=? getSquareColor (coord "d8")
        , Black ~=? getSquareColor (coord "h8")
        , White ~=? getSquareColor (coord "a2")
        , White ~=? getSquareColor (coord "b5")
        , White ~=? getSquareColor (coord "f3")
        , White ~=? getSquareColor (coord "h1")
        , White ~=? getSquareColor (coord "a8")
        ]

boardTests :: Test
boardTests = TestList [boardPrintingTests, boardCoordinateTests,
                       getPieceTest, movePieceTest, parsingBoardCompactTest,
                       isEmptyTest, isOpponentSquareTest,
                       firstPieceInSquareListTest,
                       iterateDirectionInsideBoardTest,
                       getKingSquareTest, isSquareThreatenedTest,
                       isCheckTest, getSquaresWithOwnerTest,
                       getPlayerPiecesTest, getSquareColorTest]
