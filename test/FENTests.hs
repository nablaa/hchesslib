module FENTests where

import Piece
import Board
import FEN
import Move
import Test.Hspec

fenSpec :: IO ()
fenSpec = hspec $
        describe "FEN" $ do
          writeFENSpec
          readFENSpec

writeFENSpec :: Spec
writeFENSpec =
        describe "writeFEN" $ do
          it "should serialize initial game state correctly" $ do
            writeFEN initialState `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

          it "should serialize En Passant move and move counters correctly" $ do
            writeFEN (Move.State initialBoard Black [] [] (parseCoordinate "e3") 4 14) `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b - e3 4 14"

          it "should serialize castlings correctly" $ do
            writeFEN (Move.State initialBoard White [] [Long] (parseCoordinate "c6") 0 9) `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w q c6 0 9"
            writeFEN (Move.State initialBoard White [Long] [Short] (parseCoordinate "c6") 0 9) `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Qk c6 0 9"

readFENSpec :: Spec
readFENSpec =
        describe "readFEN" $ do
          it "should read initial game state FEN correctly" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" `shouldBe` Just initialState

          it "should read FEN with castlings, En Passant and move counters correctly" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Kq e3 7 14" `shouldBe` Just (Move.State initialBoard White [Short] [Long] (parseCoordinate "e3") 7 14)
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 10 42" `shouldBe` Just (Move.State initialBoard White [] [] Nothing 10 42)

          it "should not read FEN with missing castling information" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - 0 1" `shouldBe` Nothing

          it "should not read FEN with invalid player information" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR K KQkq - 0 1" `shouldBe` Nothing

          it "should not read FEN with invalid move counter information" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - a 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 a" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - -1 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 0" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 -1" `shouldBe` Nothing

          it "should not read FEN with invalid board" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" `shouldBe` Nothing
            readFEN "nbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" `shouldBe` Nothing

          it "should not read FEN with invalid castling information" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KKkq - 0 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w qKqQ - 0 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkK - 0 1" `shouldBe` Nothing

          it "should not read FEN with invalid En Passant square" $ do
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq a9 0 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq i8 0 1" `shouldBe` Nothing
            readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq 44 0 1" `shouldBe` Nothing

          it "should not read garbage" $ do
            readFEN "foobar" `shouldBe` Nothing
            readFEN "" `shouldBe` Nothing
            readFEN "1 2 3 4 5 6" `shouldBe` Nothing
