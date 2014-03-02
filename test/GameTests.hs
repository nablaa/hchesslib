module GameTests where

import FEN
import Piece
import Move
import Game
import TestUtils
import Test.Hspec

gameSpec :: IO ()
gameSpec = hspec $
        describe "applyMove" $ do
          it "should update board, player and full move counter correctly" $ do
            testApplyingMove "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" (Movement (Piece White Pawn) (coord "e2") (coord "e3")) "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1"

          it "should update en passant square for white correctly" $ do
            testApplyingMove "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

          it "should update en passant square for black correctly" $ do
            testApplyingMove "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" (PawnDoubleMove (Piece Black Pawn) (coord "c7") (coord "c5")) "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

          it "should update halfmove counter when no capture of pawn advance" $ do
            testApplyingMove "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2" (Movement (Piece White Knight) (coord "g1") (coord "f3")) "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

          it "should zero halfmove counter piece is captured" $ do
            testApplyingMove "r1bqkb1r/pppppppp/2n2n2/4N3/8/2N5/PPPPPPPP/R1BQKB1R b KQkq - 5 3" (Capture (Piece Black Knight) (coord "c6") (coord "e5")) "r1bqkb1r/pppppppp/5n2/4n3/8/2N5/PPPPPPPP/R1BQKB1R w KQkq - 0 4"

          it "should zero halfmove counter when pawn moves" $ do
            testApplyingMove "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 5 4" (Movement (Piece Black Pawn) (coord "d7") (coord "d6")) "r1bqkb1r/ppp2ppp/2np1n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 0 5"

          it "should zero halfmove counter when pawn promotes" $ do
            testApplyingMove "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP2pP/R1BQK2R b KQkq - 1 6" (Promotion (Piece Black Pawn) (coord "g2") (coord "g1") Queen) "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP3P/R1BQK1qR w KQkq - 0 7"

          it "should zero halfmove counter when pawn promotes and captures" $ do
            testApplyingMove "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP2pP/R1BQK2R b KQkq - 1 6" (Promotion (Piece Black Pawn) (coord "g2") (coord "h1") Queen) "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP3P/R1BQK2q w Qkq - 0 7"

          it "should invalidate short castling when kingside rook moves" $ do
            testApplyingMove "rnbqk1nr/pppp2pp/5p2/2b5/2B1Pp2/5N2/PPPP2PP/RNBQK2R w KQkq - 0 5" (Movement (Piece White Rook) (coord "h1") (coord "g1")) "rnbqk1nr/pppp2pp/5p2/2b5/2B1Pp2/5N2/PPPP2PP/RNBQK1R1 b Qkq - 1 5"

          it "should invalidate long castling when queenside rook moves" $ do
            testApplyingMove "r3k1nr/pppq2pp/2npbp2/2b5/2B1PB2/2NP1N2/PPP1Q1PP/R3K1R1 b Qkq - 0 9" (Movement (Piece Black Rook) (coord "a8") (coord "d8")) "3rk1nr/pppq2pp/2npbp2/2b5/2B1PB2/2NP1N2/PPP1Q1PP/R3K1R1 w Qk - 1 10"
          it "should invalidate both castlings when king moves" $ do
            testApplyingMove "rnb1kbnr/ppppqppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3" (Movement (Piece White King) (coord "e1") (coord "e2")) "rnb1kbnr/ppppqppp/8/4p3/2B1P3/8/PPPPKPPP/RNBQ2NR b kq - 3 3"

          it "should invalidate long castling when queenside rook is captured" $ do
            testApplyingMove "r1b1kbnr/1p1npppp/R2q4/p1pp4/P7/8/1PPPPPPP/1NBQKBNR w Kkq - 4 6" (Capture (Piece White Rook) (coord "a6") (coord "a8")) "R1b1kbnr/1p1npppp/3q4/p1pp4/P7/8/1PPPPPPP/1NBQKBNR b Kk - 0 6"


testApplyingMove :: String -> Move -> String -> Expectation
testApplyingMove beforeFen move afterFen = (writeFEN newGame) `shouldBe` afterFen
        where Right newGame = applyMove (game beforeFen) move