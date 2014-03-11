module MoveTests where

import Chess.Piece
import Chess.Board
import Chess.Move
import TestUtils
import Test.Hspec

moveSpec :: IO ()
moveSpec = hspec $
        describe "Move" $ do
          isCorrectStartPieceSpec
          isRightPlayerMoveSpec
          areCoordinatesValidSpec
          generateAllRookMovesSpec
          generateAllBishopMovesSpec
          generateAllQueenMovesSpec
          generateAllKnightMovesSpec
          generateAllKingMovesSpec
          generateAllPawnMovesSpec
          generateAllPotentialMovesSpec
          boardAfterMoveSpec
          generateAllMovesSpec

isCorrectStartPieceSpec :: Spec
isCorrectStartPieceSpec =
        describe "isCorrectStartPiece" $
          it "should detect whether the piece in the coordinates matches the given piece information" $ do
             isCorrectStartPiece initialBoard (Piece White Pawn) (coord "e2") `shouldBe` True
             isCorrectStartPiece initialBoard (Piece White Knight) (coord "b1") `shouldBe` True
             isCorrectStartPiece initialBoard (Piece Black Pawn) (coord "e2") `shouldBe` False
             isCorrectStartPiece initialBoard (Piece White Bishop) (coord "e2") `shouldBe` False
             isCorrectStartPiece initialBoard (Piece White Pawn) (coord "e3") `shouldBe` False

isRightPlayerMoveSpec :: Spec
isRightPlayerMoveSpec =
        describe "isRightPlayerMove" $
          it "should detect whether the movement is by given player" $ do
             isRightPlayerMove White (Movement (Piece White Pawn) (coord "e2") (coord "e3")) `shouldBe` True
             isRightPlayerMove White (Movement (Piece Black Pawn) (coord "e7") (coord "e6")) `shouldBe` False
             isRightPlayerMove Black (Capture (Piece Black Queen) (coord "a1") (coord "a2")) `shouldBe` True
             isRightPlayerMove Black (Capture (Piece White Queen) (coord "a1") (coord "a2")) `shouldBe` False
             isRightPlayerMove White (Castling White Short) `shouldBe` True
             isRightPlayerMove Black (Castling White Long) `shouldBe` False
             isRightPlayerMove White (EnPassant (Piece White Pawn) (coord "e2") (coord "d3")) `shouldBe` True
             isRightPlayerMove Black (EnPassant (Piece White Pawn) (coord "e2") (coord "d3")) `shouldBe` False
             isRightPlayerMove Black (Promotion (Piece Black Pawn) (coord "e2") (coord "e1") Queen) `shouldBe` True
             isRightPlayerMove White (Promotion (Piece Black Pawn) (coord "e2") (coord "e1") Queen) `shouldBe` False
             isRightPlayerMove White (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) `shouldBe` True
             isRightPlayerMove Black (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) `shouldBe` False

areCoordinatesValidSpec :: Spec
areCoordinatesValidSpec =
        describe "areCoordinatesValid" $ do
          it "should return Nothing if both coordinates are inside bounds" $ do
             areCoordinatesValid (0, 0) (1, 2) `shouldBe` Nothing
             areCoordinatesValid (4, 4) (0, 7) `shouldBe` Nothing

          it "should return InvalidCoordinates error if at least one coordinate is out of bounds" $ do
             areCoordinatesValid (3, 3) (3, 3) `shouldBe` Just InvalidCoordinates
             areCoordinatesValid (-1, 0) (3, 3) `shouldBe` Just InvalidCoordinates
             areCoordinatesValid (7, 0) (7, 8) `shouldBe` Just InvalidCoordinates

generateAllRookMovesSpec :: Spec
generateAllRookMovesSpec =
        describe "generateAllRookMoves" $ do
          it "should return all movement and capture moves for a rook in a square" $
            generateAllRookMoves (game "k4b1r/p7/7p/4P3/1n2P2R/8/P2B1P2/3K2N1 w - - 6 9") (coord "h4") `shouldMatchList`
              [ Movement (Piece White Rook) (coord "h4") (coord "h5")
              , Movement (Piece White Rook) (coord "h4") (coord "g4")
              , Movement (Piece White Rook) (coord "h4") (coord "f4")
              , Movement (Piece White Rook) (coord "h4") (coord "h3")
              , Movement (Piece White Rook) (coord "h4") (coord "h2")
              , Movement (Piece White Rook) (coord "h4") (coord "h1")
              , Capture (Piece White Rook) (coord "h4") (coord "h6")]

          it "should return empty list of moves if no move is possible" $
            generateAllRookMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "a1") `shouldMatchList` []

generateAllBishopMovesSpec :: Spec
generateAllBishopMovesSpec =
        describe "generateAllBishopMoves" $ do
          it "should return all movement and capture moves for a bishop in a square" $
            generateAllBishopMoves (game "rnb1kbnr/pppp1ppp/8/5p2/8/3B4/PPPP3P/Rq1QKN2 w Qkq - 4 12") (coord "d3") `shouldMatchList`
              [ Movement (Piece White Bishop) (coord "d3") (coord "c4")
              , Movement (Piece White Bishop) (coord "d3") (coord "b5")
              , Movement (Piece White Bishop) (coord "d3") (coord "a6")
              , Movement (Piece White Bishop) (coord "d3") (coord "e4")
              , Movement (Piece White Bishop) (coord "d3") (coord "e2")
              , Capture (Piece White Bishop) (coord "d3") (coord "f5")]

          it "should return empty list of moves if no move is possible" $
            generateAllBishopMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "c1") `shouldMatchList` []

generateAllQueenMovesSpec :: Spec
generateAllQueenMovesSpec =
        describe "generateAllQueenMoves" $ do
          it "should return all movement and capture moves for a queen in a square" $ do
            generateAllQueenMoves (game "rnb1kbnr/pppp1ppp/8/5p2/8/3B4/PPPP3P/Rq1QKN2 b Qkq - 4 12") (coord "b1") `shouldMatchList`
              [ Movement (Piece Black Queen) (coord "b1") (coord "c1")
              , Capture (Piece Black Queen) (coord "b1") (coord "a1")
              , Capture (Piece Black Queen) (coord "b1") (coord "a2")
              , Capture (Piece Black Queen) (coord "b1") (coord "b2")
              , Capture (Piece Black Queen) (coord "b1") (coord "c2")
              , Capture (Piece Black Queen) (coord "b1") (coord "d1")]
            generateAllQueenMoves (game "r2qkb1r/ppnp2pp/4pp1n/4b3/2Q2P2/1P2P3/1P2BPPP/RPB1K1NR w KQkq - 2 13") (coord "c4") `shouldMatchList`
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
              , Capture (Piece White Queen) (coord "c4") (coord "e6")]

          it "should return empty list of moves if no move is possible" $
            generateAllQueenMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "d1") `shouldMatchList` []

generateAllKnightMovesSpec :: Spec
generateAllKnightMovesSpec =
        describe "generateAllKnightMoves" $
          it "should return all movement and capture moves for a knight in a square" $ do
            generateAllKnightMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "b1") `shouldMatchList`
              [ Movement (Piece White Knight) (coord "b1") (coord "a3")
              , Movement (Piece White Knight) (coord "b1") (coord "c3")]
            generateAllKnightMoves (game "r2qkbnr/2p1pppp/ppp5/bn6/3P4/2P5/PP2PPPP/RNBQKBNR b - - 3 7") (coord "b5") `shouldMatchList`
              [ Movement (Piece Black Knight) (coord "b5") (coord "d6")
              , Movement (Piece Black Knight) (coord "b5") (coord "a7")
              , Movement (Piece Black Knight) (coord "b5") (coord "a3")
              , Capture (Piece Black Knight) (coord "b5") (coord "c3")
              , Capture (Piece Black Knight) (coord "b5") (coord "d4")]

generateAllKingMovesSpec :: Spec
generateAllKingMovesSpec =
        describe "generateAllKingMoves" $ do
          it "should return empty list of moves if no move is possible" $
            generateAllKingMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "e1") `shouldMatchList` []

          it "should return all movement and capture moves for a king in a square" $
            generateAllKingMoves (game "2b5/pkp1prpp/8/rpn5/1K6/2P5/PP2N1PR/N1Q5 w - - 2 15") (coord "b4") `shouldMatchList`
              [ Movement (Piece White King) (coord "b4") (coord "a4")
              , Movement (Piece White King) (coord "b4") (coord "a3")
              , Movement (Piece White King) (coord "b4") (coord "b3")
              , Movement (Piece White King) (coord "b4") (coord "c4")
              , Capture (Piece White King) (coord "b4") (coord "a5")
              , Capture (Piece White King) (coord "b4") (coord "b5")
              , Capture (Piece White King) (coord "b4") (coord "c5")]

          it "should return castling moves when castling is possible" $
            generateAllKingMoves (game "1rq1kbr1/pppbnn1p/8/8/8/3N4/PPPP2PP/R3K2R w KQ - 0 10") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "e2")
              , Movement (Piece White King) (coord "e1") (coord "f2")
              , Movement (Piece White King) (coord "e1") (coord "f1")
              , Castling White Long
              , Castling White Short]

          it "should not return castling move if the castling has been invalidated" $ do
            generateAllKingMoves (game "r3k2r/ppppp1pp/2n2p2/3b4/8/2B2PPB/PPPPP2P/R3K2R w Qk - 5 14") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "f2")
              , Movement (Piece White King) (coord "e1") (coord "f1")
              , Castling White Long]
            generateAllKingMoves (game "r3k2r/ppppp1pp/2n2p2/3b4/8/2B2PPB/PPPPP2P/R3K2R w Qk - 5 14") (coord "e8") `shouldMatchList`
              [ Movement (Piece Black King) (coord "e8") (coord "d8")
              , Movement (Piece Black King) (coord "e8") (coord "f7")
              , Movement (Piece Black King) (coord "e8") (coord "f8")
              , Castling Black Short]

          it "should not return castling move if there is piece between castling line" $
            generateAllKingMoves (game "4k3/n7/8/8/8/5N2/P1N2PPP/R1n1K1R1 w Q - 0 16") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "d2")
              , Movement (Piece White King) (coord "e1") (coord "e2")
              , Movement (Piece White King) (coord "e1") (coord "f1")]

          it "should not return castling move if there is an opponent piece threatening castling end square" $
            generateAllKingMoves (game "3rk1r1/5p2/8/8/3b2N1/5P2/1PP1P1PP/4K2R w K - 2 20") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "d2")
              , Movement (Piece White King) (coord "e1") (coord "f1")
              , Movement (Piece White King) (coord "e1") (coord "f2")]

          it "should not return castling move if there is an opponent piece threatening a castling line square" $
            generateAllKingMoves (game "3rk3/5p2/5r2/8/3P2N1/8/1PP1P1PP/4K2R w K - 0 12") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "d2")
              , Movement (Piece White King) (coord "e1") (coord "f1")
              , Movement (Piece White King) (coord "e1") (coord "f2")]

          it "castling out of check should not be legal" $
            generateAllKingMoves (game "3rk3/5p2/4r3/8/3P2N1/8/1PP2PPP/4K2R w K - 0 12") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "d2")
              , Movement (Piece White King) (coord "e1") (coord "e2")
              , Movement (Piece White King) (coord "e1") (coord "f1")]

          it "castling when rook is threatened should be possible" $
            generateAllKingMoves (game "3rk3/5p2/7r/8/3P2N1/8/1PP2PP1/4K2R w K - 0 12") (coord "e1") `shouldMatchList`
              [ Movement (Piece White King) (coord "e1") (coord "d1")
              , Movement (Piece White King) (coord "e1") (coord "d2")
              , Movement (Piece White King) (coord "e1") (coord "e2")
              , Movement (Piece White King) (coord "e1") (coord "f1")
              , Castling White Short]

generateAllPawnMovesSpec :: Spec
generateAllPawnMovesSpec =
        describe "generateAllPawnMoves" $ do
          it "should return both normal movement and double move when possible" $ do
            generateAllPawnMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (coord "e2") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "e2") (coord "e3")
              , PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")]
            generateAllPawnMoves (game "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1") (coord "a7") `shouldMatchList`
              [ Movement (Piece Black Pawn) (coord "a7") (coord "a6")
              , PawnDoubleMove (Piece Black Pawn) (coord "a7") (coord "a5")]

          it "should return empty list when no moves are possible" $ do
            generateAllPawnMoves (game "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1") (coord "e4") `shouldMatchList` []
            generateAllPawnMoves (game "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPP2PPP/RNBQKBNR b KQkq - 0 1") (coord "e5") `shouldMatchList` []

          it "should return both movement and capture moves when possible" $
            generateAllPawnMoves (game "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPP5/RNBQKBNR w KQkq - 0 1") (coord "f4") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "f4") (coord "f5")
              , Capture (Piece White Pawn) (coord "f4") (coord "e5")]

          it "should return only capture when normal movement is not possible" $
            generateAllPawnMoves (game "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPP5/RNBQKBNR w KQkq - 0 1") (coord "e5") `shouldMatchList`
              [ Capture (Piece Black Pawn) (coord "e5") (coord "f4")]

          it "should return normal movement when capture is not possible" $
            generateAllPawnMoves (game "rnbqkbnr/pppp1p1p/8/4pPp1/4P3/8/PPPP2PP/RNBQKBNR w KQkq - 0 4") (coord "f5") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "f5") (coord "f6")]

          it "should return en passant when it is possible" $ do
            generateAllPawnMoves (game "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3") (coord "e5") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "e5") (coord "e6")
              , EnPassant (Piece White Pawn) (coord "e5") (coord "f6")]
            generateAllPawnMoves (game "rnbqkbnr/ppp1pppp/8/8/2PpPP2/8/PP1P2PP/RNBQKBNR b KQkq c3 0 3") (coord "d4") `shouldMatchList`
              [ Movement (Piece Black Pawn) (coord "d4") (coord "d3")
              , EnPassant (Piece Black Pawn) (coord "d4") (coord "c3")]

          it "should not return en passant when en passant square is not one of the capture squares" $
            generateAllPawnMoves (game "rnbqkbnr/1pppp3/p5pp/3PPp2/8/8/PPP2PPP/RNBQKBNR w KQkq f6 0 5") (coord "d5") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "d5") (coord "d6")]

          it "should return all promotion moves when promotion with normal movement or by capture is possible" $ do
            generateAllPawnMoves (game "1r5k/2P5/8/8/8/8/8/4K2N w - - 0 1") (coord "c7") `shouldMatchList`
              [ Promotion (Piece White Pawn) (coord "c7") (coord "c8") Rook
              , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Knight
              , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Bishop
              , Promotion (Piece White Pawn) (coord "c7") (coord "c8") Queen
              , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Rook
              , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Knight
              , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Bishop
              , Promotion (Piece White Pawn) (coord "c7") (coord "b8") Queen]
            generateAllPawnMoves (game "k7/8/8/8/8/8/p7/1N2K3 w - - 0 1") (coord "a2") `shouldMatchList`
              [ Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Rook
              , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Knight
              , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Bishop
              , Promotion (Piece Black Pawn) (coord "a2") (coord "a1") Queen
              , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Rook
              , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Knight
              , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Bishop
              , Promotion (Piece Black Pawn) (coord "a2") (coord "b1") Queen]

          it "should not return any moves if no move is possible" $ do
            generateAllPawnMoves (game "k7/8/8/8/8/8/p7/N3K3 w - - 0 1") (coord "a2") `shouldMatchList` []
            generateAllPawnMoves (game "k7/8/8/8/8/4b3/4P3/K7 w - - 0 1") (coord "e2") `shouldMatchList` []

          it "should generate normal movement correctly" $
            generateAllPawnMoves (game "6k1/8/8/8/8/3P4/8/4K3 w - - 0 1") (coord "d3") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "d3") (coord "d4")]

generateAllPotentialMovesSpec :: Spec
generateAllPotentialMovesSpec =
        describe "generateAllPotentialMoves" $
          it "should return all potential moves for all pieces of the current player" $ do
            generateAllPotentialMoves (game "8/k7/8/8/6p1/7P/6PK/8 w - - 0 1") `shouldMatchList`
              [ Movement (Piece White Pawn) (coord "g2") (coord "g3")
              , Movement (Piece White Pawn) (coord "h3") (coord "h4")
              , Capture (Piece White Pawn) (coord "h3") (coord "g4")
              , Movement (Piece White King) (coord "h2") (coord "g3")
              , Movement (Piece White King) (coord "h2") (coord "h1")
              , Movement (Piece White King) (coord "h2") (coord "g1")]
            generateAllPotentialMoves (game "8/k7/8/8/6p1/7P/6PK/8 b - - 0 1") `shouldMatchList`
              [ Capture (Piece Black Pawn) (coord "g4") (coord "h3")
              , Movement (Piece Black Pawn) (coord "g4") (coord "g3")
              , Movement (Piece Black King) (coord "a7") (coord "a8")
              , Movement (Piece Black King) (coord "a7") (coord "a6")
              , Movement (Piece Black King) (coord "a7") (coord "b8")
              , Movement (Piece Black King) (coord "a7") (coord "b7")
              , Movement (Piece Black King) (coord "a7") (coord "b6")]

boardAfterMoveSpec :: Spec
boardAfterMoveSpec =
        describe "boardAfterMove" $ do
          it "should make pawn double move correctly" $ do
            boardAfterMove (fenBoard "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") (PawnDoubleMove (Piece White Pawn) (coord "e2") (coord "e4")) `shouldBe` Just (fenBoard "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1")
            boardAfterMove (fenBoard "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 1") (PawnDoubleMove (Piece Black Pawn) (coord "c7") (coord "c5")) `shouldBe` Just (fenBoard "rnbqkbnr/p2p1ppp/p7/2p1p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq c6 0 2")
            boardAfterMove (fenBoard "rnbqkbnr/p2p1ppp/p7/4N3/2p1P3/8/PPPP1PPP/RNBQ1RK1 w kq - 0 3") (PawnDoubleMove (Piece White Pawn) (coord "d2") (coord "d4")) `shouldBe` Just (fenBoard "rnbqkbnr/p2p1ppp/p7/4N3/2pPP3/8/PPP2PPP/RNBQ1RK1 b kq d3 0 3")

          it "should make pawn normal move correctly" $ do
            boardAfterMove (fenBoard "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1") (Movement (Piece White Knight) (coord "g1") (coord "f3")) `shouldBe` Just (fenBoard "rnbqkbnr/pppp1ppp/8/4p3/4P3/5N2/PPPP1PPP/RNBQKB1R w KQkq - 0 1")
            boardAfterMove (fenBoard "rnbqkbnr/p2p1ppp/p7/2p1N3/4P3/8/PPPP1PPP/RNBQ1RK1 b kq - 0 2") (Movement (Piece Black Pawn) (coord "c5") (coord "c4")) `shouldBe` Just (fenBoard "rnbqkbnr/p2p1ppp/p7/4N3/2p1P3/8/PPPP1PPP/RNBQ1RK1 w kq - 0 3")

          it "should make pawn capture move correctly" $
            boardAfterMove (fenBoard "rnbqkbnr/pppp1ppp/B7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1") (Capture (Piece Black Pawn) (coord "b7") (coord "a6")) `shouldBe` Just (fenBoard "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1")

          it "should move king and rook correctly in castling move" $
            boardAfterMove (fenBoard "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1") (Castling White Short) `shouldBe` Just (fenBoard "rnbqkbnr/p1pp1ppp/p7/4p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 1 1")

          it "should make knight's capture move correctly" $
            boardAfterMove (fenBoard "rnbqkbnr/p2p1ppp/p7/2p1p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 w kq c6 0 2") (Capture (Piece White Knight) (coord "f3") (coord "e5")) `shouldBe` Just (fenBoard "rnbqkbnr/p2p1ppp/p7/2p1N3/4P3/8/PPPP1PPP/RNBQ1RK1 b kq - 0 2")

          it "should make en passant move correctly" $
            boardAfterMove (fenBoard "rnbqkbnr/p2p1ppp/p7/4N3/2pPP3/8/PPP2PPP/RNBQ1RK1 b kq d3 0 3") (EnPassant (Piece Black Pawn) (coord "c4") (coord "d3")) `shouldBe` Just (fenBoard "rnbqkbnr/p2p1ppp/p7/4N3/4P3/3p4/PPP2PPP/RNBQ1RK1 w kq - 0 4")

generateAllMovesSpec :: Spec
generateAllMovesSpec =
        describe "generateAllMoves" $ do
          it "should generate all legal moves for the current player in a given game state" $
            generateAllMoves (game "rnbqkb1r/pp1p1Bpp/5n2/2p1p3/4P3/2P5/PP1P1PPP/RNBQK1NR b KQkq - 0 4") `shouldMatchList`
              [Movement (Piece Black King) (0,4) (1,4), Capture (Piece Black King) (0,4) (1,5)]

          it "should not give any legal moves in checkmate" $ do
            generateAllMoves (game "8/5r2/4K1q1/4p3/3k4/8/8/8 w - - 0 7") `shouldBe` []
            generateAllMoves (game "4r2r/p6p/1pnN2p1/kQp5/3pPq2/3P4/PPP3PP/R5K1 b - - 0 2") `shouldBe` []
            generateAllMoves (game "r3k2r/ppp2p1p/2n1p1p1/8/2B2P1q/2NPb1n1/PP4PP/R2Q3K w kq - 0 8") `shouldBe` []
            generateAllMoves (game "8/6R1/pp1r3p/6p1/P3R1Pk/1P4P1/7K/8 b - - 0 4") `shouldBe` []

          it "should not give any legal moves in stalemate" $ do
            generateAllMoves (game "1R6/8/8/8/8/8/7R/k6K b - - 0 1") `shouldBe` []
            generateAllMoves (game "8/8/5k2/p4p1p/P4K1P/1r6/8/8 w - - 0 2") `shouldBe` []
