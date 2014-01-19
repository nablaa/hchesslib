module PieceTests where

import Piece
import Test.HUnit

printPieceTests :: Test
printPieceTests = TestList [
          "K" ~=? printPiece (Piece White King)
        , "q" ~=? printPiece (Piece Black Queen)
        , "p" ~=? printPiece (Piece Black Pawn)
        , "R" ~=? printPiece (Piece White Rook)
        , "b" ~=? printPiece (Piece Black Bishop)
        , "N" ~=? printPiece (Piece White Knight)
        ]

parsePieceTests :: Test
parsePieceTests = TestList [
          Just (Piece Black King) ~=? parsePiece 'k'
        , Just (Piece White Queen) ~=? parsePiece 'Q'
        , Just (Piece White Pawn) ~=? parsePiece 'P'
        , Just (Piece Black Rook) ~=? parsePiece 'r'
        , Just (Piece White Rook) ~=? parsePiece 'R'
        , Just (Piece White Bishop) ~=? parsePiece 'B'
        , Just (Piece Black Knight) ~=? parsePiece 'n'
        , Nothing ~=? parsePiece 'a'
        , Nothing ~=? parsePiece 'c'
        , Nothing ~=? parsePiece 'd'
        , Nothing ~=? parsePiece '0'
        , Nothing ~=? parsePiece '5'
        , Nothing ~=? parsePiece '!'
        ]

pieceTests :: Test
pieceTests = TestList [printPieceTests, parsePieceTests]
