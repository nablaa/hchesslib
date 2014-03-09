module NotationTests where

import Piece
import Move
import Notation
import TestUtils
import Test.Hspec

notationSpec :: IO ()
notationSpec = hspec $
        describe "Notation" parseCoordinateNotationSpec

parseCoordinateNotationSpec :: Spec
parseCoordinateNotationSpec =
        describe "parseCoordinateNotation" $ do
          it "should not parse invalid input" $ do
            parseCoordinateNotation initialState "" `shouldBe` Nothing
            parseCoordinateNotation initialState "e" `shouldBe` Nothing
            parseCoordinateNotation initialState "e2" `shouldBe` Nothing
            parseCoordinateNotation initialState "e2-" `shouldBe` Nothing
            parseCoordinateNotation initialState "e2-e" `shouldBe` Nothing
            parseCoordinateNotation initialState "e2-4" `shouldBe` Nothing
            parseCoordinateNotation initialState "fooba" `shouldBe` Nothing
            parseCoordinateNotation initialState "i2-i4" `shouldBe` Nothing
            parseCoordinateNotation initialState "12-34" `shouldBe` Nothing

          it "should not parse illegal moves" $ do
            parseCoordinateNotation initialState "b1-d2" `shouldBe` Nothing
            parseCoordinateNotation initialState "e7-e5" `shouldBe` Nothing

          it "should not parse moves with extra input" $ do
            parseCoordinateNotation initialState "e2-e4foo" `shouldBe` Nothing
            parseCoordinateNotation initialState "e2-e4=Qfoo" `shouldBe` Nothing

          it "should parse correctly pawn moves" $ do
            parseCoordinateNotation initialState "e2-e4" `shouldBe` Just (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4"))
            parseCoordinateNotation initialState "D2-D3" `shouldBe` Just (Movement (Piece White Pawn) (coord "d2") (coord "d3"))
            parseCoordinateNotation initialState "H2-H4" `shouldBe` Just (PawnDoubleMove (Piece White Pawn) (coord "h2") (coord "h4"))
            parseCoordinateNotation (game "r1bqkbnr/pppp1ppp/2n5/4p3/3PP3/5N2/PPP2PPP/RNBQKB1R b KQkq d3 0 3") "e5-d4" `shouldBe` Just (Capture (Piece Black Pawn) (coord "e5") (coord "d4"))

          it "should parse correctly knight moves" $
            parseCoordinateNotation initialState "b1-c3" `shouldBe` Just (Movement (Piece White Knight) (coord "b1") (coord "c3"))

          it "should parse correctly castling moves" $ do
            parseCoordinateNotation (game "r1b1k2r/pppq1ppp/2np1n2/2b1p3/2BPPB2/2N2N2/PPP2PPP/R2QK2R w KQkq - 4 7") "e1-g1" `shouldBe` Just (Castling White Short)
            parseCoordinateNotation (game "r1b1k2r/p1pq1ppp/1pnp1n2/2b1p3/2BPPB2/2N2N2/PPPQ1PPP/R3K2R w KQkq - 0 8") "e1-c1" `shouldBe` Just (Castling White Long)

          it "should parse correctly pawn promotion moves" $ do
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=N" `shouldBe` Just (Promotion (Piece White Pawn) (coord "c7") (coord "c8") Knight)
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=Q" `shouldBe` Just (Promotion (Piece White Pawn) (coord "c7") (coord "c8") Queen)
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-b8=Q" `shouldBe` Just (Promotion (Piece White Pawn) (coord "c7") (coord "b8") Queen)
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8(N)" `shouldBe` Just (Promotion (Piece White Pawn) (coord "c7") (coord "c8") Knight)
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8(Q)" `shouldBe` Just (Promotion (Piece White Pawn) (coord "c7") (coord "c8") Queen)
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8(Q" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8Q" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8(Q))" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8" `shouldBe` Nothing

          it "should not parse pawn promotion moves with illegal promotion piece" $ do
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=A" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=n" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=K" `shouldBe` Nothing
            parseCoordinateNotation (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") "c7-c8=P" `shouldBe` Nothing
