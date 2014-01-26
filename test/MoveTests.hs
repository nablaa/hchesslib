module MoveTests where

import Piece
import Board
import Move
import Data.Maybe
import Test.HUnit

isCorrectStartPieceTests :: Test
isCorrectStartPieceTests = TestList [
          Nothing ~=? isCorrectStartPiece initialBoard (Piece White Pawn) (coord "e2")
        , Nothing ~=? isCorrectStartPiece initialBoard (Piece White Knight) (coord "b1")
        , Just WrongPiece ~=? isCorrectStartPiece initialBoard (Piece Black Pawn) (coord "e2")
        , Just WrongPiece ~=? isCorrectStartPiece initialBoard (Piece White Bishop) (coord "e2")
        , Just WrongPiece ~=? isCorrectStartPiece initialBoard (Piece White Pawn) (coord "e3")
        ]

isRightPlayerMoveTests :: Test
isRightPlayerMoveTests = TestList [
          Nothing ~=? isRightPlayerMove White (Movement (Piece White Pawn) (coord "e2") (coord "e3"))
        , Just WrongPlayer ~=? isRightPlayerMove White (Movement (Piece Black Pawn) (coord "e7") (coord "e6"))
        , Nothing ~=? isRightPlayerMove Black (Capture (Piece Black Queen) (coord "a1") (coord "a2"))
        , Just WrongPlayer ~=? isRightPlayerMove Black (Capture (Piece White Queen) (coord "a1") (coord "a2"))
        , Nothing ~=? isRightPlayerMove White (Castling White Short)
        , Just WrongPlayer ~=? isRightPlayerMove Black (Castling White Long)
        , Nothing ~=? isRightPlayerMove White (EnPassant (Piece White Pawn) (coord "e2") (coord "d3"))
        , Just WrongPlayer ~=? isRightPlayerMove Black (EnPassant (Piece White Pawn) (coord "e2") (coord "d3"))
        , Nothing ~=? isRightPlayerMove Black (Promotion (Piece Black Pawn) (coord "e2") (coord "e1") Queen)
        , Just WrongPlayer ~=? isRightPlayerMove White (Promotion (Piece Black Pawn) (coord "e2") (coord "e1") Queen)
        , Nothing ~=? isRightPlayerMove White (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e3"))
        , Just WrongPlayer ~=? isRightPlayerMove Black (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e3"))
        ]

areCoordinatesValidTests :: Test
areCoordinatesValidTests = TestList [
          Nothing ~=? areCoordinatesValid (0, 0) (1, 2)
        , Nothing ~=? areCoordinatesValid (4, 4) (0, 7)
        , Just InvalidCoordinates ~=? areCoordinatesValid (3, 3) (3, 3)
        , Just InvalidCoordinates ~=? areCoordinatesValid (-1, 0) (3, 3)
        , Just InvalidCoordinates ~=? areCoordinatesValid (7, 0) (7, 8)
        ]

coord :: String -> Coordinates
coord = fromJust . parseCoordinate


moveTests :: Test
moveTests = TestList [isCorrectStartPieceTests, isRightPlayerMoveTests, areCoordinatesValidTests]
