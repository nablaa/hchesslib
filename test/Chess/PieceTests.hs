module PieceTests where

import Chess.Piece
import Test.Hspec

pieceSpec :: IO ()
pieceSpec = hspec $
        describe "Piece" $ do
          printPieceSpec
          parsePieceSpec

printPieceSpec :: Spec
printPieceSpec =
        describe "printPiece" $ do
          it "should print white player pieces uppercase" $ do
            printPiece (Piece White King) `shouldBe` "K"
            printPiece (Piece White Rook) `shouldBe` "R"
            printPiece (Piece White Knight) `shouldBe` "N"

          it "should print black player pieces lowercase" $ do
            printPiece (Piece Black Queen) `shouldBe` "q"
            printPiece (Piece Black Pawn) `shouldBe` "p"
            printPiece (Piece Black Bishop) `shouldBe` "b"

parsePieceSpec :: Spec
parsePieceSpec =
        describe "parsePiece" $ do
          it "should parse legal piecetypes correctly" $ do
            parsePiece 'k' `shouldBe` Just (Piece Black King)
            parsePiece 'Q' `shouldBe` Just (Piece White Queen)
            parsePiece 'P' `shouldBe` Just (Piece White Pawn)
            parsePiece 'r' `shouldBe` Just (Piece Black Rook)
            parsePiece 'R' `shouldBe` Just (Piece White Rook)
            parsePiece 'B' `shouldBe` Just (Piece White Bishop)
            parsePiece 'n' `shouldBe` Just (Piece Black Knight)

          it "should not parse invalid piecetypes" $ do
            parsePiece 'a' `shouldBe` Nothing
            parsePiece 'c' `shouldBe` Nothing
            parsePiece 'd' `shouldBe` Nothing
            parsePiece '0' `shouldBe` Nothing
            parsePiece '5' `shouldBe` Nothing
            parsePiece '!' `shouldBe` Nothing
