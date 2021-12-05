module UITests where

import Chess
import Chess.UI
import Test.Hspec

uiSpec :: IO ()
uiSpec = hspec $
        describe "UI" $ do
          printBoardSpec

printBoardSpec :: Spec
printBoardSpec =
        describe "printBoard" $ do
          it "should print the initial board correctly" $
            printBoard (board newGame) `shouldBe` "  +---+---+---+---+---+---+---+---+\n8 | r | n | b | q | k | b | n | r |\n  +---+---+---+---+---+---+---+---+\n7 | p | p | p | p | p | p | p | p |\n  +---+---+---+---+---+---+---+---+\n6 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n5 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n4 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n3 |   |   |   |   |   |   |   |   |\n  +---+---+---+---+---+---+---+---+\n2 | P | P | P | P | P | P | P | P |\n  +---+---+---+---+---+---+---+---+\n1 | R | N | B | Q | K | B | N | R |\n  +---+---+---+---+---+---+---+---+\n    a   b   c   d   e   f   g   h\n"
