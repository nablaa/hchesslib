module GameTests where

import Chess.FEN
import Chess.Internal.Piece
import Chess.Internal.Move
import Chess.Internal.Game
import TestUtils
import Test.Hspec

gameSpec :: IO ()
gameSpec = hspec $
        describe "Game" $ do
          applyMoveSpec
          isCheckmateSpec
          isStalemateSpec
          isInsufficientMaterialSpec
          isDrawSpec
          getWinnerSpec

applyMoveSpec :: Spec
applyMoveSpec =
          describe "applyMove" $ do
            it "should update board, player and full move counter correctly" $
              testApplyingMove "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" (Movement (Piece White Pawn) (coord "e2") (coord "e3")) "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1"

            it "should update en passant square for white correctly" $
              testApplyingMove "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

            it "should update en passant square for black correctly" $
              testApplyingMove "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1" (PawnDoubleMove (Piece Black Pawn) (coord "c7") (coord "c5")) "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

            it "should update halfmove counter when no capture of pawn advance" $
              testApplyingMove "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2" (Movement (Piece White Knight) (coord "g1") (coord "f3")) "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"

            it "should zero halfmove counter piece is captured" $
              testApplyingMove "r1bqkb1r/pppppppp/2n2n2/4N3/8/2N5/PPPPPPPP/R1BQKB1R b KQkq - 5 3" (Capture (Piece Black Knight) (coord "c6") (coord "e5")) "r1bqkb1r/pppppppp/5n2/4n3/8/2N5/PPPPPPPP/R1BQKB1R w KQkq - 0 4"

            it "should zero halfmove counter when pawn moves" $
              testApplyingMove "r1bqkb1r/pppp1ppp/2n2n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 5 4" (Movement (Piece Black Pawn) (coord "d7") (coord "d6")) "r1bqkb1r/ppp2ppp/2np1n2/4p3/2B1P3/2N2N2/PPPP1PPP/R1BQK2R w KQkq - 0 5"

            it "should zero halfmove counter when pawn promotes" $
              testApplyingMove "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP2pP/R1BQK2R b KQkq - 1 6" (Promotion (Piece Black Pawn) (coord "g2") (coord "g1") Queen) "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP3P/R1BQK1qR w KQkq - 0 7"

            it "should zero halfmove counter when pawn promotes and captures" $
              testApplyingMove "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP2pP/R1BQK2R b KQkq - 1 6" (Promotion (Piece Black Pawn) (coord "g2") (coord "h1") Queen) "rnbqkbnr/ppp2ppp/B7/4N3/8/2N5/PPPP3P/R1BQK2q w Qkq - 0 7"

            it "should invalidate short castling when kingside rook moves" $
              testApplyingMove "rnbqk1nr/pppp2pp/5p2/2b5/2B1Pp2/5N2/PPPP2PP/RNBQK2R w KQkq - 0 5" (Movement (Piece White Rook) (coord "h1") (coord "g1")) "rnbqk1nr/pppp2pp/5p2/2b5/2B1Pp2/5N2/PPPP2PP/RNBQK1R1 b Qkq - 1 5"

            it "should invalidate long castling when queenside rook moves" $
              testApplyingMove "r3k1nr/pppq2pp/2npbp2/2b5/2B1PB2/2NP1N2/PPP1Q1PP/R3K1R1 b Qkq - 0 9" (Movement (Piece Black Rook) (coord "a8") (coord "d8")) "3rk1nr/pppq2pp/2npbp2/2b5/2B1PB2/2NP1N2/PPP1Q1PP/R3K1R1 w Qk - 1 10"

            it "should invalidate both castlings when king moves" $
              testApplyingMove "rnb1kbnr/ppppqppp/8/4p3/2B1P3/8/PPPP1PPP/RNBQK1NR w KQkq - 2 3" (Movement (Piece White King) (coord "e1") (coord "e2")) "rnb1kbnr/ppppqppp/8/4p3/2B1P3/8/PPPPKPPP/RNBQ2NR b kq - 3 3"

            it "should invalidate long castling when queenside rook is captured" $
              testApplyingMove "r1b1kbnr/1p1npppp/R2q4/p1pp4/P7/8/1PPPPPPP/1NBQKBNR w Kkq - 4 6" (Capture (Piece White Rook) (coord "a6") (coord "a8")) "R1b1kbnr/1p1npppp/3q4/p1pp4/P7/8/1PPPPPPP/1NBQKBNR b Kk - 0 6"

isCheckmateSpec :: Spec
isCheckmateSpec =
          describe "isCheckmate" $ do
            it "should not consider initial state as checkmate" $
              isCheckmate initialState `shouldBe` False

            it "should not consider check a checkmate" $
              isCheckmate (game "rnbqkb1r/ppp2ppp/3p1n2/1B2p3/4P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 1 4") `shouldBe` False

            it "should not consider stalemates a checkmate" $ do
              isCheckmate (game "1R6/8/8/8/8/8/7R/k6K b - - 0 1") `shouldBe` False
              isCheckmate (game "8/8/5k2/p4p1p/P4K1P/1r6/8/8 w - - 0 2") `shouldBe` False

            it "should detect checkmate correctly" $ do
              isCheckmate (game "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") `shouldBe` True
              isCheckmate (game "4r2r/p6p/1pnN2p1/kQp5/3pPq2/3P4/PPP3PP/R5K1 b - - 0 2") `shouldBe` True
              isCheckmate (game "r3k2r/ppp2p1p/2n1p1p1/8/2B2P1q/2NPb1n1/PP4PP/R2Q3K w kq - 0 8") `shouldBe` True
              isCheckmate (game "8/6R1/pp1r3p/6p1/P3R1Pk/1P4P1/7K/8 b - - 0 4") `shouldBe` True

isStalemateSpec :: Spec
isStalemateSpec =
          describe "isStalemate" $ do
            it "should not consider initial state as stalemate" $
              isStalemate initialState `shouldBe` False

            it "should not consider check a stalemate" $
              isStalemate (game "rnbqkb1r/ppp2ppp/3p1n2/1B2p3/4P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 1 4") `shouldBe` False

            it "should detect stalemate correctly" $ do
              isStalemate (game "1R6/8/8/8/8/8/7R/k6K b - - 0 1") `shouldBe` True
              isStalemate (game "8/8/5k2/p4p1p/P4K1P/1r6/8/8 w - - 0 2") `shouldBe` True

isInsufficientMaterialSpec :: Spec
isInsufficientMaterialSpec =
          describe "isInsufficientMaterial" $ do
            it "should not consider initial state as insufficient material" $
              isInsufficientMaterial initialState `shouldBe` False

            it "should not consider queen+king vs. queen+king as insufficient material" $
              isInsufficientMaterial (game "3qk3/8/8/8/8/8/8/3QK3 w - - 0 10") `shouldBe` False

            it "should not consider rook+king vs. king as insufficient material" $ do
              isInsufficientMaterial (game "4k3/8/8/8/8/8/8/3RK3 w - - 0 10") `shouldBe` False
              isInsufficientMaterial (game "4k3/2r5/8/8/8/8/8/4K3 w - - 0 1") `shouldBe` False

            it "should not consider pawn+king vs. king as insufficient material" $ do
              isInsufficientMaterial (game "4k3/8/8/8/8/8/1P6/4K3 w - - 0 10") `shouldBe` False
              isInsufficientMaterial (game "4k3/2p5/8/8/8/8/8/4K3 w - - 0 1") `shouldBe` False

            it "should not consider knight+bishop+king vs. king as insufficient material" $
              isInsufficientMaterial (game "4k3/8/8/8/8/8/2BN4/4K3 w - - 0 1") `shouldBe` False

            it "should not consider bishop+bishop+king vs. king as insufficient material if bishops are on different colored squares" $
              isInsufficientMaterial (game "4k3/8/8/8/8/8/2BB4/4K3 w - - 0 1") `shouldBe` False

            it "should consider bishop+bishop+king vs. king as insufficient material if bishops are on same colored squares" $
              isInsufficientMaterial (game "4k3/8/8/8/8/2B5/3B4/4K3 w - - 0 1") `shouldBe` True

            it "should not consider knight+knight+king vs. king as insufficient material" $
              isInsufficientMaterial (game "4k3/8/8/8/8/8/2NN4/4K3 w - - 0 1") `shouldBe` False

            it "should consider king vs. king as insufficient material" $
              isInsufficientMaterial (game "4k3/8/8/8/8/8/8/4K3 w - - 0 10") `shouldBe` True

            it "should consider bishop+king vs. king as insufficient material" $ do
              isInsufficientMaterial (game "4k3/8/8/8/8/8/8/3BK3 w - - 0 10") `shouldBe` True
              isInsufficientMaterial (game "4k3/2b5/8/8/8/8/8/4K3 w - - 0 1") `shouldBe` True

            it "should consider knight+king vs. king as insufficient material" $ do
              isInsufficientMaterial (game "4k3/8/8/8/8/8/8/3NK3 w - - 0 10") `shouldBe` True
              isInsufficientMaterial (game "4k3/2n5/8/8/8/8/8/4K3 w - - 0 1") `shouldBe` True

            it "should consider bishop+king vs. bishop+king as insufficient material if both bishops are on same color square" $
              isInsufficientMaterial (game "4k3/8/3b4/8/8/8/1B6/4K3 w - - 0 10") `shouldBe` True

            it "should consider n*bishop+king vs. n*bishop+king as insufficient material if bishop pairs are on same color square" $
              isInsufficientMaterial (game "8/b1B1b1B1/1b1B1b1B/8/8/8/8/1k5K w - - 0 1") `shouldBe` True

            it "should not consider n*bishop+king vs. n*bishop+king as insufficient material if bishop pairs are on different color square" $
              isInsufficientMaterial (game "8/bB2b1B1/1b1B1b1B/8/8/8/8/1k5K w - - 0 1") `shouldBe` False

            it "should not consider bishop+king vs. bishop+king as insufficient material if both bishops are on different color square" $
              isInsufficientMaterial (game "4k3/8/4b3/8/8/8/1B6/4K3 w - - 0 10") `shouldBe` False

isDrawSpec :: Spec
isDrawSpec =
                describe "isDraw" $ do
                  it "should not detect normal game as draw" $
                    isDraw initialState `shouldBe` False

                  it "should not detect checkmate as draw" $
                    isDraw (game "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") `shouldBe` False

                  it "should detect stalemate as draw" $
                    isDraw (game "1R6/8/8/8/8/8/7R/k6K b - - 0 1") `shouldBe` True

                  it "should detect insufficient material  as draw" $
                    isDraw (game "4k3/8/8/8/8/8/8/4K3 w - - 0 10") `shouldBe` True

testApplyingMove :: String -> Move -> String -> Expectation
testApplyingMove beforeFen move afterFen = writeFEN newGame `shouldBe` afterFen
        where Right newGame = applyMove (game beforeFen) move

getWinnerSpec :: Spec
getWinnerSpec =
                describe "getWinner" $ do
                  it "should not give winner if there is no checkmate" $
                    getWinner (game "rnbqkb1r/ppp2ppp/3p1n2/1B2p3/4P3/2N2N2/PPPP1PPP/R1BQK2R b KQkq - 1 4") `shouldBe` Nothing

                  it "should not give winner if the game is draw" $
                    getWinner (game "1R6/8/8/8/8/8/7R/k6K b - - 0 1") `shouldBe` Nothing

                  it "should return correct winner if the game is checkmate" $ do
                    getWinner (game "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") `shouldBe` Just Black
                    getWinner (game "4r2r/p6p/1pnN2p1/kQp5/3pPq2/3P4/PPP3PP/R5K1 b - - 0 2") `shouldBe` Just White
