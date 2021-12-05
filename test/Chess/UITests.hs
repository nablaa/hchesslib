module UITests where

import Chess
import Chess.UI
import Chess.Internal.Move
import Test.Hspec

uiSpec :: IO ()
uiSpec = hspec $
        describe "UI" $ do
          printBoardSpec
          printMoveSpec

printBoardSpec :: Spec
printBoardSpec =
        describe "printBoard" $ do
          it "should print the initial board correctly" $
            printBoard (board newGame) `shouldBe` newGamePrint

newGamePrint :: String
newGamePrint = "  +---+---+---+---+---+---+---+---+\n8 | r | n | b | q | k | b | n | r |\n  +---+---+---+---+---+---+---+---+\n7 | p | p | p | p | p | p | p | p |\n  +---+---+---+---+---+---+---+---+\n6 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n5 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n4 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n3 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n2 | P | P | P | P | P | P | P | P |\n  +---+---+---+---+---+---+---+---+\n1 | R | N | B | Q | K | B | N | R |\n  +---+---+---+---+---+---+---+---+\n    a   b   c   d   e   f   g   h\n"

printMoveSpec :: Spec
printMoveSpec =
        describe "printMove" $ do
          it "should print normal moves correctly" $ do
            coordinateNotation (Movement (Piece White Pawn) (6, 4) (5, 4)) `shouldBe` "e2-e3"
            coordinateNotation (Capture (Piece White Pawn) (6, 4) (5, 5)) `shouldBe` "e2-f3"
            coordinateNotation (Capture (Piece Black Knight) (0, 1) (2, 2)) `shouldBe` "b8-c6"
            coordinateNotation (Castling White Short) `shouldBe` "e1-g1"
            coordinateNotation (Castling White Long) `shouldBe` "e1-c1"
            coordinateNotation (Castling Black Short) `shouldBe` "e8-g8"
            coordinateNotation (Castling Black Long) `shouldBe` "e8-c8"
            coordinateNotation (EnPassant (Piece White Pawn) (3, 4) (2, 5)) `shouldBe` "e5-f6"
            coordinateNotation (PawnDoubleMove (Piece White Pawn) (6, 1) (4, 1)) `shouldBe` "b2-b4"

          it "should print promotion moves correctly" $ do
            coordinateNotation (Promotion (Piece White Pawn) (1, 0) (0, 0) Queen) `shouldBe` "a7-a8q"
            coordinateNotation (Promotion (Piece Black Pawn) (6, 1) (7, 2) Knight) `shouldBe` "b2-c1n"
