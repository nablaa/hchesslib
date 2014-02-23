module MoveTests where

import Piece
import Board
import Move
import FEN
import Data.Maybe
import qualified Data.Set as S
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

generateAllRookMovesTests :: Test
generateAllRookMovesTests = TestList [testGeneratingMoves generateAllRookMoves
                                     "k4b1r/p7/7p/4P3/1n2P2R/8/P2B1P2/3K2N1 w - - 6 9" "h4"
                                     [ Movement (Piece White Rook) (coord "h4") (coord "h5")
                                     , Movement (Piece White Rook) (coord "h4") (coord "g4")
                                     , Movement (Piece White Rook) (coord "h4") (coord "f4")
                                     , Movement (Piece White Rook) (coord "h4") (coord "h3")
                                     , Movement (Piece White Rook) (coord "h4") (coord "h2")
                                     , Movement (Piece White Rook) (coord "h4") (coord "h1")
                                     , Capture (Piece White Rook) (coord "h4") (coord "h6")
                                     ],
                                     testGeneratingMoves generateAllRookMoves
                                     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "a1"
                                     []]

generateAllBishopMovesTests :: Test
generateAllBishopMovesTests = TestList [testGeneratingMoves generateAllBishopMoves
                                       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "c1"
                                       [],
                                       testGeneratingMoves generateAllBishopMoves
                                       "rnb1kbnr/pppp1ppp/8/5p2/8/3B4/PPPP3P/Rq1QKN2 w Qkq - 4 12" "d3"
                                       [ Movement (Piece White Bishop) (coord "d3") (coord "c4")
                                       , Movement (Piece White Bishop) (coord "d3") (coord "b5")
                                       , Movement (Piece White Bishop) (coord "d3") (coord "a6")
                                       , Movement (Piece White Bishop) (coord "d3") (coord "e4")
                                       , Movement (Piece White Bishop) (coord "d3") (coord "e2")
                                       , Capture (Piece White Bishop) (coord "d3") (coord "f5")
                                       ]]

generateAllQueenMovesTests :: Test
generateAllQueenMovesTests = TestList [testGeneratingMoves generateAllQueenMoves
                                      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "d1"
                                      [],
                                      testGeneratingMoves generateAllQueenMoves
                                      "rnb1kbnr/pppp1ppp/8/5p2/8/3B4/PPPP3P/Rq1QKN2 b Qkq - 4 12" "b1"
                                      [ Movement (Piece Black Queen) (coord "b1") (coord "c1")
                                      , Capture (Piece Black Queen) (coord "b1") (coord "a1")
                                      , Capture (Piece Black Queen) (coord "b1") (coord "a2")
                                      , Capture (Piece Black Queen) (coord "b1") (coord "b2")
                                      , Capture (Piece Black Queen) (coord "b1") (coord "c2")
                                      , Capture (Piece Black Queen) (coord "b1") (coord "d1")
                                      ],
                                      testGeneratingMoves generateAllQueenMoves
                                      "r2qkb1r/ppnp2pp/4pp1n/4b3/2Q2P2/1P2P3/1P2BPPP/RPB1K1NR w KQkq - 2 13" "c4"
                                      [ Movement (Piece White Queen) (coord "c4") (coord "b5")
                                      , Movement (Piece White Queen) (coord "c4") (coord "a6")
                                      , Movement (Piece White Queen) (coord "c4") (coord "d5")
                                      , Movement (Piece White Queen) (coord "c4") (coord "d3")
                                      , Movement (Piece White Queen) (coord "c4") (coord "b4")
                                      , Movement (Piece White Queen) (coord "c4") (coord "a4")
                                      , Movement (Piece White Queen) (coord "c4") (coord "c5")
                                      , Movement (Piece White Queen) (coord "c4") (coord "c6")
                                      , Movement (Piece White Queen) (coord "c4") (coord "d4")
                                      , Movement (Piece White Queen) (coord "c4") (coord "e4")
                                      , Movement (Piece White Queen) (coord "c4") (coord "c3")
                                      , Movement (Piece White Queen) (coord "c4") (coord "c2")
                                      , Capture (Piece White Queen) (coord "c4") (coord "c7")
                                      , Capture (Piece White Queen) (coord "c4") (coord "e6")
                                      ]]

testGeneratingMoves :: (GameState -> Coordinates -> [Move]) -> String -> String -> [Move] -> Test
testGeneratingMoves func fen square moves = TestList [
          TestLabel ("Correct move count for: '" ++ fen ++ "' => " ++ square) (length moves ~=? length generated)
        , TestLabel ("Correct move list for: '" ++ fen ++ "' => " ++ square) (S.fromList moves ~=? S.fromList generated)
        ]
        where generated = func (game fen) (coord square)

coord :: String -> Coordinates
coord = fromJust . parseCoordinate

game :: String -> GameState
game = fromJust . readFEN


moveTests :: Test
moveTests = TestList [isCorrectStartPieceTests, isRightPlayerMoveTests, areCoordinatesValidTests,
                      generateAllRookMovesTests, generateAllBishopMovesTests, generateAllQueenMovesTests]
