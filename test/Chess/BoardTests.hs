module BoardTests where

import Chess.Piece
import Chess.Board
import Data.Maybe
import TestUtils
import Test.Hspec

boardSpec :: IO ()
boardSpec = hspec $
        describe "Board" $ do
          boardPrintingSpec
          isInsideBoardSpec
          parseCoordinateSpec
          printCoordinateSpec
          getPieceSpec
          movePieceSpec
          parseBoardCompactSpec
          isEmptySpec
          isOpponentSquareSpec
          firstPieceInSquareListSpec
          iterateDirectionInsideBoardSpec
          getKingSquareSpec
          isSquareThreatenedSpec
          isCheckSpec
          getSquaresWithOwnerSpec
          getPlayerPiecesSpec
          getSquareColorSpec
          getSquaresWithPiecesSpec

boardPrintingSpec :: Spec
boardPrintingSpec =
        describe "printBoardCompact" $ do
          it "should print initial board position correctly" $
            printBoardCompact initialBoard `shouldBe` "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"

          it "should print empty board correctly" $
            printBoardCompact emptyBoard `shouldBe` "        \n        \n        \n        \n        \n        \n        \n        \n"

isInsideBoardSpec :: Spec
isInsideBoardSpec =
        describe "isInsideBoard" $
          it "should detect correctly whether given coordinates are inside board bounds" $do
            isInsideBoard (0, 0) `shouldBe` True
            isInsideBoard (7, 7) `shouldBe` True
            isInsideBoard (3, 5) `shouldBe` True
            isInsideBoard (-1, 5) `shouldBe` False
            isInsideBoard (3, -1) `shouldBe` False
            isInsideBoard (8, 5) `shouldBe` False
            isInsideBoard (3, 8) `shouldBe` False

parseCoordinateSpec :: Spec
parseCoordinateSpec =
        describe "parseCoordinate" $ do
          it "should parse legal coordinates correctly" $ do
            parseCoordinate "a8" `shouldBe` Just (0, 0)
            parseCoordinate "b8" `shouldBe` Just (0, 1)
            parseCoordinate "c8" `shouldBe` Just (0, 2)
            parseCoordinate "a7" `shouldBe` Just (1, 0)
            parseCoordinate "a6" `shouldBe` Just (2, 0)
            parseCoordinate "a1" `shouldBe` Just (7, 0)
            parseCoordinate "h8" `shouldBe` Just (0, 7)
            parseCoordinate "h1" `shouldBe` Just (7, 7)
            parseCoordinate "f5" `shouldBe` Just (3, 5)

          it "should parse not parse invalid coordinates" $ do
            parseCoordinate "B5" `shouldBe` Nothing
            parseCoordinate "F5" `shouldBe` Nothing
            parseCoordinate "12" `shouldBe` Nothing
            parseCoordinate "i1" `shouldBe` Nothing
            parseCoordinate "a9" `shouldBe` Nothing
            parseCoordinate "a-1" `shouldBe` Nothing
            parseCoordinate "foobar" `shouldBe` Nothing

printCoordinateSpec :: Spec
printCoordinateSpec =
        describe "printCoordinate" $
          it "should print legal coordinates correctly" $ do
            printCoordinate (0, 0) `shouldBe` "a8"
            printCoordinate (0, 1) `shouldBe` "b8"
            printCoordinate (0, 2) `shouldBe` "c8"
            printCoordinate (1, 0) `shouldBe` "a7"
            printCoordinate (2, 0) `shouldBe` "a6"
            printCoordinate (3, 5) `shouldBe` "f5"

getPieceSpec :: Spec
getPieceSpec =
        describe "getPiece" $ do
          it "should return Nothing for invalid coordinates" $ do
            getPiece initialBoard (-1, -1) `shouldBe` Nothing
            getPiece initialBoard (8, 8) `shouldBe` Nothing

          it "should return Nothing for empty squares" $
            getPiece initialBoard (4, 4) `shouldBe` Nothing

          it "should return piece for squares that have pieces" $ do
            getPiece initialBoard (0, 0) `shouldBe` Just (Piece Black Rook)
            getPiece initialBoard (0, 3) `shouldBe` Just (Piece Black Queen)
            getPiece initialBoard (7, 4) `shouldBe` Just (Piece White King)

movePieceSpec :: Spec
movePieceSpec =
        describe "movePiece" $ do
          it "should return Nothing for invalid coordinates" $ do
             movePiece (0, 0) (8, 8) initialBoard `shouldBe` Nothing
             movePiece (-1, -1) (7, 7) initialBoard `shouldBe` Nothing

          it "should return Nothing when start square does not have a piece" $
             movePiece (4, 4) (0, 0) initialBoard `shouldBe` Nothing

          it "should return new board when start square has a piece and end coordinate is valid" $ do
             printBoardCompact (fromJust (movePiece (0, 0) (4, 4) initialBoard)) `shouldBe` " nbqkbnr\npppppppp\n        \n        \n    r   \n        \nPPPPPPPP\nRNBQKBNR\n"
             printBoardCompact (fromJust (movePiece (0, 0) (0, 0) initialBoard)) `shouldBe` "rnbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"
             printBoardCompact (fromJust (movePiece (0, 0) (0, 1) initialBoard)) `shouldBe` " rbqkbnr\npppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n"

parseBoardCompactSpec :: Spec
parseBoardCompactSpec =
        describe "parseBoardCompact" $ do
          it "should be able to parse the initial board" $
             parseBoardCompact (printBoardCompact initialBoard) `shouldBe` Just initialBoard

          it "should be able to parse the empty board" $
            parseBoardCompact "        \n        \n        \n        \n        \n        \n        \n        \n" `shouldBe` Just emptyBoard

          it "should return Nothing with invalid board (too short row)" $
            parseBoardCompact "        \n        \n        \n        \n        \n        \n        \n        \n        \n" `shouldBe` Nothing

          it "should return Nothing with invalid board (too few rows)" $
            parseBoardCompact "         \n        \n        \n        \n        \n        \n        \n        \n" `shouldBe` Nothing

          it "should return Nothing with empty input" $
            parseBoardCompact "" `shouldBe` Nothing

          it "should return Nothing with garbage input" $
            parseBoardCompact "foobar" `shouldBe` Nothing

          it "should return Nothing with invalid board (invalid pieces)" $ do
             parseBoardCompact "        \n  xxx   \n        \n        \n        \n        \n        \n        \n" `shouldBe` Nothing
             parseBoardCompact "rnbqkbnr\nppxppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" `shouldBe` Nothing

          it "should return Nothing with invalid board (empty row)" $
             parseBoardCompact "rnbqkbnr\npppppppp\n        \n\n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" `shouldBe` Nothing

          it "should return Nothing with invalid board (missing row)" $
             parseBoardCompact "pppppppp\n        \n        \n        \n        \nPPPPPPPP\nRNBQKBNR\n" `shouldBe` Nothing

isEmptySpec :: Spec
isEmptySpec =
        describe "isEmpty" $ do
          it "should return True for empty squares" $
             isEmpty initialBoard (4, 4) `shouldBe` True

          it "should return True for squares out of bounds" $ do
             isEmpty initialBoard (-1, -1) `shouldBe` True
             isEmpty initialBoard (8, 8) `shouldBe` True

          it "should return False for squares containing pieces" $ do
             isEmpty initialBoard (0, 0) `shouldBe` False
             isEmpty initialBoard (0, 3) `shouldBe` False
             isEmpty initialBoard (7, 4) `shouldBe` False

isOpponentSquareSpec :: Spec
isOpponentSquareSpec =
        describe "isOpponentSquare" $ do
          it "should return False for empty squares" $
             isOpponentSquare initialBoard (4, 4) White `shouldBe` False

          it "should return False for squares out of bounds" $ do
             isOpponentSquare initialBoard (-1, -1) Black `shouldBe` False
             isOpponentSquare initialBoard (8, 8) White `shouldBe` False

          it "should return False for squares that have own pieces" $ do
             isOpponentSquare initialBoard (0, 0) Black `shouldBe` False
             isOpponentSquare initialBoard (7, 4) White `shouldBe` False

          it "should return True for squares that have opponent pieces" $ do
             isOpponentSquare initialBoard (0, 3) White `shouldBe` True
             isOpponentSquare initialBoard (7, 4) Black `shouldBe` True

firstPieceInSquareListSpec :: Spec
firstPieceInSquareListSpec =
        describe "firstPieceInSquareList" $ do
          it "should return Nothing if none of the coordinates contain any piece" $
             firstPieceInSquareList initialBoard [(4, 1), (4, 2), (4, 3)] `shouldBe` Nothing

          it "should return the piece of the first coordinate from the list that contains a piece" $ do
             firstPieceInSquareList initialBoard [(4, 1), (5, 2), (6, 3), (7, 4)] `shouldBe` Just (Piece White Pawn)
             firstPieceInSquareList initialBoard [(7, 4), (0, 4)] `shouldBe` Just (Piece White King)

iterateDirectionInsideBoardSpec :: Spec
iterateDirectionInsideBoardSpec =
        describe "iterateDirectionInsideBoard" $ do
          it "should return empty list for square next to border with direction towards border" $
             iterateDirectionInsideBoard (4, 0) (0, -1) `shouldBe` []

          it "should return the list of coordinates that you get when you start walking from a square towards a certain direction" $ do
             iterateDirectionInsideBoard (4, 1) (-1, 0) `shouldBe` [(3,1),(2,1),(1,1),(0,1)]
             iterateDirectionInsideBoard (4, 1) (1, 1) `shouldBe` [(5,2),(6,3),(7,4)]

getKingSquareSpec :: Spec
getKingSquareSpec =
        describe "getKingSquare" $
          it "should return the coordinates of the square where king is currently" $ do
            getKingSquare initialBoard White `shouldBe` coord "e1"
            getKingSquare initialBoard Black `shouldBe` coord "e8"

isSquareThreatenedSpec :: Spec
isSquareThreatenedSpec =
        describe "isSquareThreatened" $ do
          it "should return False for player square if no opponent piece threatens the given square" $ do
             isSquareThreatened initialBoard Black (coord "e1") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g2") `shouldBe` False

          it "should return False for empty square if no opponent piece threatens the given square" $ do
             isSquareThreatened initialBoard Black (coord "e4") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "h5") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d8") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "f4") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d8") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "h6") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "b5") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "c4") `shouldBe` False

          it "should return True for player square if player piece threatens the given square" $
             isSquareThreatened initialBoard White (coord "e1") `shouldBe` True

          it "should not consider Pawn movement square threatened" $ do
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "c6") `shouldBe` False
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "e5") `shouldBe` False

          it "should detect when Pawn threatens a square" $ do
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "f1") `shouldBe` True
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "h1") `shouldBe` True

          it "should detect when Knight threatens a square" $ do
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g5") `shouldBe` True
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "b4") `shouldBe` True
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "f4") `shouldBe` True

          it "should detect when Bishop threatens a square" $
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g8") `shouldBe` True

          it "should detect when Queen threatens a square" $
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "b1") `shouldBe` True

          it "should detect when King threatens a square" $
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") Black (coord "d8") `shouldBe` True

          it "should detect when multiple pieces threaten a square" $ do
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "d5") `shouldBe` True
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "g4") `shouldBe` True
             isSquareThreatened (fenBoard "rnb1kb1r/ppp1p2p/4Bp2/2Pp4/1q2P1n1/7N/P1NP1PpP/R1BQK2R w KQkq - 0 1") White (coord "f1") `shouldBe` True

isCheckSpec :: Spec
isCheckSpec =
        describe "isCheck" $ do
          it "should return False for initial board" $ do
             isCheck initialBoard White `shouldBe` False
             isCheck initialBoard Black `shouldBe` False

          it "should return True when opponent threatens the king square" $ do
             isCheck (fenBoard "rnbqkbnr/pppp2pp/5p2/4p2Q/4P3/3P4/PPP2PPP/RNB1KBNR b KQkq - 1 3") Black `shouldBe` True
             isCheck (fenBoard "rnbqk2r/pppp2bp/3N2pn/4pp1Q/4P3/3P4/PPP2PPP/RNB1KB1R b KQkq - 1 7") Black `shouldBe` True
             isCheck (fenBoard "rnbq3r/pppp2bp/3Nk1pn/3Ppp1Q/4P3/8/PPP2PPP/RNB1KB1R b KQ - 0 9") Black `shouldBe` True
             isCheck (fenBoard "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") White `shouldBe` True
             isCheck (fenBoard "r3k2r/ppp2p1p/2n1p1p1/8/2B2P1q/2NPb1n1/PP4PP/R2Q3K w kq - 0 8") White `shouldBe` True

          it "should return False when king square is not threatened by the opponent" $ do
             isCheck (fenBoard "rnbqkbnr/pppp2pp/5p2/4p2Q/4P3/3P4/PPP2PPP/RNB1KBNR b KQkq - 1 3") White `shouldBe` False
             isCheck (fenBoard "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") Black `shouldBe` False

getSquaresWithOwnerSpec :: Spec
getSquaresWithOwnerSpec =
        describe "getSquaresWithOwner" $
          it "should return coordinates which have player pieces" $ do
             getSquaresWithOwner (fenBoard "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1") Black `shouldMatchList` [coord "a7", coord "g4"]
             getSquaresWithOwner (fenBoard "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1") White `shouldMatchList` [coord "g2", coord "h2", coord "h3"]

getPlayerPiecesSpec :: Spec
getPlayerPiecesSpec =
        describe "getPlayerPieces" $
          it "should return list of pieces a player owns" $ do
             getPlayerPieces (fenBoard "4k3/8/5np1/8/8/2BB4/2Q5/4K3 w - - 0 1") White `shouldMatchList` [Bishop, Bishop, Queen, King]
             getPlayerPieces (fenBoard "4k3/8/5np1/8/8/2BB4/2Q5/4K3 w - - 0 1") Black `shouldMatchList` [Knight, Pawn, King]

getSquareColorSpec :: Spec
getSquareColorSpec =
        describe "getSquareColor" $
          it "should return the color of the square in the given coordinate" $ do
             getSquareColor (coord "a1") `shouldBe` Black
             getSquareColor (coord "c1") `shouldBe` Black
             getSquareColor (coord "g5") `shouldBe` Black
             getSquareColor (coord "d8") `shouldBe` Black
             getSquareColor (coord "h8") `shouldBe` Black
             getSquareColor (coord "a2") `shouldBe` White
             getSquareColor (coord "b5") `shouldBe` White
             getSquareColor (coord "f3") `shouldBe` White
             getSquareColor (coord "h1") `shouldBe` White
             getSquareColor (coord "a8") `shouldBe` White

getSquaresWithPiecesSpec :: Spec
getSquaresWithPiecesSpec =
        describe "getSquaresWithPieces" $
          it "should return list of coordinates that have given piecetype on them" $ do
             getSquaresWithPieces initialBoard Rook `shouldMatchList` map coord ["a1", "h1", "a8", "h8"]
             getSquaresWithPieces initialBoard Queen `shouldMatchList` map coord ["d1", "d8"]
