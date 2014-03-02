module MoveTests where

import Piece
import Board
import Move
import qualified Data.Set as S
import Test.HUnit
import TestUtils
import Data.Maybe

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

generateAllKnightMovesTests :: Test
generateAllKnightMovesTests = TestList [testGeneratingMoves generateAllKnightMoves
                                       "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "b1"
                                       [ Movement (Piece White Knight) (coord "b1") (coord "a3")
                                       , Movement (Piece White Knight) (coord "b1") (coord "c3")
                                       ],
                                       testGeneratingMoves generateAllKnightMoves
                                       "r2qkbnr/2p1pppp/ppp5/bn6/3P4/2P5/PP2PPPP/RNBQKBNR b - - 3 7" "b5"
                                       [ Movement (Piece Black Knight) (coord "b5") (coord "d6")
                                       , Movement (Piece Black Knight) (coord "b5") (coord "a7")
                                       , Movement (Piece Black Knight) (coord "b5") (coord "a3")
                                       , Capture (Piece Black Knight) (coord "b5") (coord "c3")
                                       , Capture (Piece Black Knight) (coord "b5") (coord "d4")
                                       ]]

generateAllKingMovesTests :: Test
generateAllKingMovesTests = TestList [testGeneratingMoves generateAllKingMoves
                                     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e1"
                                     [],
                                     testGeneratingMoves generateAllKingMoves
                                     "2b5/pkp1prpp/8/rpn5/1K6/2P5/PP2N1PR/N1Q5 w - - 2 15" "b4"
                                     [ Movement (Piece White King) (coord "b4") (coord "a4")
                                     , Movement (Piece White King) (coord "b4") (coord "a3")
                                     , Movement (Piece White King) (coord "b4") (coord "b3")
                                     , Movement (Piece White King) (coord "b4") (coord "c4")
                                     , Capture (Piece White King) (coord "b4") (coord "a5")
                                     , Capture (Piece White King) (coord "b4") (coord "b5")
                                     , Capture (Piece White King) (coord "b4") (coord "c5")
                                     ],
                                     testGeneratingMoves generateAllKingMoves
                                     "1rq1kbr1/pppbnn1p/8/8/8/3N4/PPPP2PP/R3K2R w KQ - 0 10" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "e2")
                                     , Movement (Piece White King) (coord "e1") (coord "f2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     , Castling White Long
                                     , Castling White Short
                                     ],
                                     -- Castlings invalidated
                                     testGeneratingMoves generateAllKingMoves
                                     "r3k2r/ppppp1pp/2n2p2/3b4/8/2B2PPB/PPPPP2P/R3K2R w Qk - 5 14" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "f2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     , Castling White Long
                                     ],
                                     testGeneratingMoves generateAllKingMoves
                                     "r3k2r/ppppp1pp/2n2p2/3b4/8/2B2PPB/PPPPP2P/R3K2R w Qk - 5 14" "e8"
                                     [ Movement (Piece Black King) (coord "e8") (coord "d8")
                                     , Movement (Piece Black King) (coord "e8") (coord "f7")
                                     , Movement (Piece Black King) (coord "e8") (coord "f8")
                                     , Castling Black Short
                                     ],
                                     -- Piece between castling line
                                     testGeneratingMoves generateAllKingMoves
                                     "4k3/n7/8/8/8/5N2/P1N2PPP/R1n1K1R1 w Q - 0 16" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "d2")
                                     , Movement (Piece White King) (coord "e1") (coord "e2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     ],
                                     -- Piece checking castling end square
                                     testGeneratingMoves generateAllKingMoves
                                     "3rk1r1/5p2/8/8/3b2N1/5P2/1PP1P1PP/4K2R w K - 2 20" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "d2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     , Movement (Piece White King) (coord "e1") (coord "f2")
                                     ],
                                     -- Piece checking castling middle square
                                     testGeneratingMoves generateAllKingMoves
                                     "3rk3/5p2/5r2/8/3P2N1/8/1PP1P1PP/4K2R w K - 0 12" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "d2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     , Movement (Piece White King) (coord "e1") (coord "f2")
                                     ],
                                     -- Castling out of chess not legal
                                     testGeneratingMoves generateAllKingMoves
                                     "3rk3/5p2/4r3/8/3P2N1/8/1PP2PPP/4K2R w K - 0 12" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "d2")
                                     , Movement (Piece White King) (coord "e1") (coord "e2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     ],
                                     -- Castling when rook is under attack is legal
                                     testGeneratingMoves generateAllKingMoves
                                     "3rk3/5p2/7r/8/3P2N1/8/1PP2PP1/4K2R w K - 0 12" "e1"
                                     [ Movement (Piece White King) (coord "e1") (coord "d1")
                                     , Movement (Piece White King) (coord "e1") (coord "d2")
                                     , Movement (Piece White King) (coord "e1") (coord "e2")
                                     , Movement (Piece White King) (coord "e1") (coord "f1")
                                     , Castling White Short
                                     ]
                                     ]

generateAllPawnMovesTests :: Test
generateAllPawnMovesTests = TestList [testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "e2"
                                     [ Movement (Piece White Pawn) (coord "e2") (coord "e3")
                                     , PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1" "a7"
                                     [ Movement (Piece Black Pawn) (coord "a7") (coord "a6")
                                     , PawnDoubleMove (Piece Black Pawn) (coord "a7") (coord "a5")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1" "e4"
                                     [],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPP2PPP/RNBQKBNR b KQkq - 0 1" "e5"
                                     [],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPP5/RNBQKBNR w KQkq - 0 1" "f4"
                                     [ Movement (Piece White Pawn) (coord "f4") (coord "f5")
                                     , Capture (Piece White Pawn) (coord "f4") (coord "e5")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPP5/RNBQKBNR w KQkq - 0 1" "e5"
                                     [ Capture (Piece Black Pawn) (coord "e5") (coord "f4")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/pppp1p1p/8/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq - 0 4" "f5"
                                     [ Movement (Piece White Pawn) (coord "f5") (coord "f6")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3" "e5"
                                     [ Movement (Piece White Pawn) (coord "e5") (coord "e6")
                                     , EnPassant (Piece White Pawn) (coord "e5") (coord "f6")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/ppp1pppp/8/8/2PpPP2/8/PP1P2PP/RNBQKBNR b KQkq c3 0 3" "d4"
                                     [ Movement (Piece Black Pawn) (coord "d4") (coord "d3")
                                     , EnPassant (Piece Black Pawn) (coord "d4") (coord "c3")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "rnbqkbnr/1pppp3/p5pp/3PPp2/8/8/PPP2PPP/RNBQKBNR w KQkq f6 0 5" "d5"
                                     [ Movement (Piece White Pawn) (coord "d5") (coord "d6")
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1" "c7"
                                     [ Promotion (Piece White Pawn) (coord "c7") (coord "c8") Rook
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Knight
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Bishop
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Queen
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Rook
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Knight
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Bishop
                                     , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Queen
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "k7/8/8/8/8/8/p7/1N2K3 w - - 0 1" "a2"
                                     [ Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Rook
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Knight
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Bishop
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Queen
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Rook
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Knight
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Bishop
                                     , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Queen
                                     ],
                                     testGeneratingMoves generateAllPawnMoves
                                     "k7/8/8/8/8/8/p7/N3K3 w - - 0 1" "a2"
                                     [],
                                     testGeneratingMoves generateAllPawnMoves
                                     "k7/8/8/8/8/4b3/4P3/K7 w - - 0 1" "e2"
                                     [],
                                     testGeneratingMoves generateAllPawnMoves
                                     "6k1/8/8/8/8/3P4/8/4K3 w - - 0 1" "d3"
                                     [ Movement (Piece White Pawn) (coord "d3") (coord "d4")
                                     ]
                                     ]

generateAllPotentialMovesTests :: Test
generateAllPotentialMovesTests = TestList [
        S.fromList [
                  Movement (Piece White Pawn) (coord "g2") (coord "g3")
                , Movement (Piece White Pawn) (coord "h3") (coord "h4")
                , Capture (Piece White Pawn) (coord "h3") (coord "g4")
                , Movement (Piece White King) (coord "h2") (coord "g3")
                , Movement (Piece White King) (coord "h2") (coord "h1")
                , Movement (Piece White King) (coord "h2") (coord "g1")
                ] ~=? S.fromList (generateAllPotentialMoves (game "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1"))
        , S.fromList [
                  Capture (Piece Black Pawn) (coord "g4") (coord "h3")
                , Movement (Piece Black Pawn) (coord "g4") (coord "g3")
                , Movement (Piece Black King) (coord "a7") (coord "a8")
                , Movement (Piece Black King) (coord "a7") (coord "a6")
                , Movement (Piece Black King) (coord "a7") (coord "b8")
                , Movement (Piece Black King) (coord "a7") (coord "b7")
                , Movement (Piece Black King) (coord "a7") (coord "b6")
                ] ~=? S.fromList (generateAllPotentialMoves (game "8/k7/8/8/6p1/7P/6PK/8 b - - 0 1"))
        ]

testGeneratingMoves :: (GameState -> Coordinates -> [Move]) -> String -> String -> [Move] -> Test
testGeneratingMoves func fen square moves = TestList [
          TestLabel ("Correct move count for: '" ++ fen ++ "' => " ++ square) (length moves ~=? length generated)
        , TestLabel ("Correct move list for: '" ++ fen ++ "' => " ++ square) (S.fromList moves ~=? S.fromList generated)
        ]
        where generated = func (game fen) (coord square)

boardAfterMoveTests :: Test
boardAfterMoveTests = TestList [
          boardMoveTest (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1"
        , boardMoveTest (Movement (Piece White Knight) (coord "g1") (coord "f3")) "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1" "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1"
        , boardMoveTest (Capture (Piece Black Pawn) (coord "b7") (coord "a6")) "rnbqkbnr/pppp1ppp/B7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"  "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1"
        , boardMoveTest (Castling White Short) "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1" "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 1"
        , boardMoveTest (PawnDoubleMove (Piece Black Pawn) (coord "c7") (coord "c5")) "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 1" "rnbqkbnr/p2p1ppp/p7/2p1p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq c6 0 2"
        , boardMoveTest (Capture (Piece White Knight) (coord "f3") (coord "e5")) "rnbqkbnr/p2p1ppp/p7/2p1p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq c6 0 2" "rnbqkbnr/p2p1ppp/p7/2p1N3/4P3/8/PPPP1PPP/RNBQ1RK1 b kq - 0 2"
        , boardMoveTest (Movement (Piece Black Pawn) (coord "c5") (coord "c4")) "rnbqkbnr/p2p1ppp/p7/2p1N3/4P3/8/PPPP1PPP/RNBQ1RK1 b kq - 0 2" "rnbqkbnr/p2p1ppp/p7/4N3/2p1P3/8/PPPP1PPP/RNBQ1RK1 w kq - 0 3"
        , boardMoveTest (PawnDoubleMove (Piece White Pawn) (coord "d2") (coord "d4")) "rnbqkbnr/p2p1ppp/p7/4N3/2p1P3/8/PPPP1PPP/RNBQ1RK1 w kq - 0 3" "rnbqkbnr/p2p1ppp/p7/4N3/2pPP3/8/PPP2PPP/RNBQ1RK1 b kq d3 0 3"
        , boardMoveTest (EnPassant (Piece Black Pawn) (coord "c4") (coord "d3")) "rnbqkbnr/p2p1ppp/p7/4N3/2pPP3/8/PPP2PPP/RNBQ1RK1 b kq d3 0 3" "rnbqkbnr/p2p1ppp/p7/4N3/4P3/3p4/PPP2PPP/RNBQ1RK1 w kq - 0 4"
        ]

boardMoveTest :: Move -> String -> String -> Test
boardMoveTest move fenBefore fenAfter = boardAfter ~=? fromJust (boardAfterMove boardBefore move)
        where boardAfter = fenBoard fenAfter
              boardBefore = fenBoard fenBefore


moveTests :: Test
moveTests = TestList [isCorrectStartPieceTests, isRightPlayerMoveTests, areCoordinatesValidTests,
                      generateAllRookMovesTests, generateAllBishopMovesTests, generateAllQueenMovesTests,
                      generateAllKnightMovesTests, generateAllKingMovesTests, generateAllPawnMovesTests,
                      generateAllPotentialMovesTests, boardAfterMoveTests]
