module Move (GameState(..), Move(..), CastlingType(..),
             MoveError(..), isRightPlayerMove,
             isLegalMove, applyMove, initialState,
             isCorrectStartPiece, areCoordinatesValid,
             generateAllRookMoves, iterateMovementSquares,
             iterateDirection, generateAllBishopMoves,
             generateAllQueenMoves, generateAllKnightMoves,
             generateAllKingMoves, generateAllPawnMoves,
             isSquareThreatened)  where

import Piece
import Board
import Data.Maybe

data GameState = State {
      stateBoard :: Board
    , currentPlayer :: Color
    , whiteCastlingsPossible :: [CastlingType]
    , blackCastlingsPossible :: [CastlingType]
    , enPassantSquare :: Maybe Coordinates
    , halfmoveClock :: Integer
    , moveNumber :: Integer
    } deriving (Eq, Show)

data Move = Movement Piece Coordinates Coordinates
          | Capture Piece Coordinates Coordinates
          | Castling Color CastlingType
          | EnPassant Piece Coordinates Coordinates
          | Promotion Piece Coordinates Coordinates PieceType
          | PawnDoubleMove Piece Coordinates Coordinates
          deriving (Show, Eq, Ord)

data CastlingType = Short | Long
                  deriving (Show, Eq, Ord)

data MoveError = WrongPlayer | WrongPiece | InvalidCoordinates
               deriving (Show, Eq)

initialState :: GameState
initialState = State initialBoard White [Short, Long] [Short, Long] Nothing 0 1

isLegalMove :: GameState -> Move -> Bool
isLegalMove = undefined

applyMove :: GameState -> Move -> Either MoveError GameState
applyMove = undefined

isCorrectStartPiece :: Board -> Piece -> Coordinates -> Maybe MoveError
isCorrectStartPiece board (Piece color pieceType) coordinates
        = case boardPiece of
                  Nothing -> Just WrongPiece
                  Just (Piece color' pieceType') -> if color == color' && pieceType == pieceType'
                                                    then Nothing
                                                    else Just WrongPiece
        where boardPiece = getPiece board coordinates

isRightPlayerMove :: Color -> Move -> Maybe MoveError
isRightPlayerMove player (Movement (Piece color _) _ _) = rightPlayer player color
isRightPlayerMove player (Capture (Piece color _) _ _) = rightPlayer player color
isRightPlayerMove player (Castling color _) = rightPlayer player color
isRightPlayerMove player (EnPassant (Piece color _) _ _) = rightPlayer player color
isRightPlayerMove player (Promotion (Piece color _) _ _ _) = rightPlayer player color
isRightPlayerMove player (PawnDoubleMove (Piece color _) _ _) = rightPlayer player color

rightPlayer :: Color -> Color -> Maybe MoveError
rightPlayer player color | player == color = Nothing
                         | otherwise = Just WrongPlayer

areCoordinatesValid :: Coordinates -> Coordinates -> Maybe MoveError
areCoordinatesValid start end | start == end = Just InvalidCoordinates
                              | not (isInsideBoard start) || not (isInsideBoard end) = Just InvalidCoordinates
                              | otherwise = Nothing

generateAllRookMoves :: GameState -> Coordinates -> [Move]
generateAllRookMoves game coords = patternMoves game coords rookPattern

generateAllBishopMoves :: GameState -> Coordinates -> [Move]
generateAllBishopMoves game coords = patternMoves game coords bishopPattern

generateAllQueenMoves :: GameState -> Coordinates -> [Move]
generateAllQueenMoves game coords = patternMoves game coords (rookPattern ++ bishopPattern)

generateAllKnightMoves :: GameState -> Coordinates -> [Move]
generateAllKnightMoves game coords = map (\coordinate -> Movement piece coords coordinate) emptySquares
                                     ++ map (\coordinate -> Capture piece coords coordinate) opponentSquares
        where squares = filter isInsideBoard [squareDiff coords jump | jump <- knightPattern]
              board = stateBoard game
              emptySquares = filter (isEmpty board) squares
              piece@(Piece player _) = fromJust $ getPiece board coords
              opponentSquares = filter (\square -> isOpponentSquare board square player) squares

generateAllKingMoves :: GameState -> Coordinates -> [Move]
generateAllKingMoves game coords = map (\coordinate -> Movement piece coords coordinate) moveSquares
                                   ++ map (\coordinate -> Capture piece coords coordinate) captureSquares
                                   ++ kingCastlingMoves game coords
        where board = stateBoard game
              piece@(Piece player _) = fromJust $ getPiece board coords
              moveSquares = kingMoveSquares game coords
              captureSquares = kingCaptureSquares game coords

generateAllPawnMoves :: GameState -> Coordinates -> [Move]
generateAllPawnMoves game coords = []

kingMoveSquares :: GameState -> Coordinates -> [Coordinates]
kingMoveSquares game start = emptySquares
        where allSquares = map (squareDiff start) (rookPattern ++ bishopPattern)
              board = stateBoard game
              emptySquares = filter (isEmpty board) $ filter isInsideBoard allSquares

kingCaptureSquares :: GameState -> Coordinates -> [Coordinates]
kingCaptureSquares game start = opponentSquares
        where allSquares = map (squareDiff start) (rookPattern ++ bishopPattern)
              board = stateBoard game
              piece@(Piece player _) = fromJust $ getPiece board start
              opponentSquares = filter (\square -> isOpponentSquare board square player) $ filter isInsideBoard allSquares

kingCastlingMoves :: GameState -> Coordinates -> [Move]
kingCastlingMoves game coords = map (Castling player) $ filter (isCastlingPossible game player) [Long, Short]
        where board = stateBoard game
              (Piece player _) = fromJust $ getPiece board coords

isCastlingPossible :: GameState -> Color -> CastlingType -> Bool
isCastlingPossible game player castling = castling `elem` possibleCastlings player && all (isEmpty board) squares && traverseSquaresNotThreatened
        where squares = castlingSquares player castling
              board = stateBoard game
              possibleCastlings White = whiteCastlingsPossible game
              possibleCastlings Black = blackCastlingsPossible game
              kingSquare = if player == White then (7, 4) else (0, 4)
              traverseSquaresNotThreatened = not $ any (isSquareThreatened game (opponent player)) (kingSquare : squares)

castlingSquares :: Color -> CastlingType -> [Coordinates]
castlingSquares White Long = [(7, 1), (7, 2), (7, 3)]
castlingSquares White Short = [(7, 5), (7, 6)]
castlingSquares Black Long = [(0, 1), (0, 2), (0, 3)]
castlingSquares Black Short = [(0, 5), (0, 6)]

rookPattern :: [(Int, Int)]
rookPattern = [(-1, 0), (1, 0), (0, -1), (0, 1)]

bishopPattern :: [(Int, Int)]
bishopPattern = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

knightPattern :: [(Int, Int)]
knightPattern = [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]

patternMoves :: GameState -> Coordinates -> [(Int, Int)] -> [Move]
patternMoves game start pattern
        = concat [movementsInDirection game start dir | dir <- pattern]
        ++ concat [capturesInDirection game start dir | dir <- pattern]

movementsInDirection :: GameState -> Coordinates -> (Int, Int) -> [Move]
movementsInDirection game start direction = map (\coordinate -> Movement piece start coordinate) squares
        where piece = fromJust $ getPiece (stateBoard game) start
              squares = iterateMovementSquares game start direction

capturesInDirection :: GameState -> Coordinates -> (Int, Int) -> [Move]
capturesInDirection game start direction = map (\coordinate -> Capture piece start coordinate) squares
        where piece = fromJust $ getPiece (stateBoard game) start
              squares = iterateCaptureSquares game start direction

iterateMovementSquares :: GameState -> Coordinates -> (Int, Int) -> [Coordinates]
iterateMovementSquares game start direction = iterateDirection (isEmptySquare) game start direction
        where isEmptySquare game coord = isEmpty (stateBoard game) coord

iterateCaptureSquares :: GameState -> Coordinates -> (Int, Int) -> [Coordinates]
iterateCaptureSquares game start direction = case squaresNotEmpty of
                                                     [] -> []
                                                     (first:_) -> if isOpponentSquare board first player then [first]
                                                                                                         else []
        where squares = iterateDirectionInsideBoard start direction
              board = stateBoard game
              (Piece player _) = fromJust $ getPiece board start
              squaresNotEmpty = dropWhile (isEmpty board) squares

iterateDirection :: (GameState -> Coordinates -> Bool) -> GameState -> Coordinates -> (Int, Int) -> [Coordinates]
iterateDirection condition game start direction = takeWhile (condition game) squares
        where squares = iterateDirectionInsideBoard start direction

squareDiff :: (Int, Int) -> (Int, Int) -> (Int, Int)
squareDiff (xd, yd) (x, y) = (xd + x, yd + y)

isSquareThreatened :: GameState -> Color -> Coordinates -> Bool
isSquareThreatened game opponentPlayer coords = knightsThreaten || pawnsThreaten || otherPiecesThreaten || kingsThreaten || rookOrQueenThreatens || bishopOrQueenThreatens
        where board = stateBoard game
              knightSquares = map (squareDiff coords) knightPattern
              knightsThreaten = any isOpponentKnight knightSquares
              isOpponentKnight square = case getPiece board square of
                                                Just (Piece player Knight) -> player == opponentPlayer
                                                _ -> False
              pawnsThreaten = any isOpponentPawn $ map (squareDiff coords) pawnSquares
              pawnSquares = case opponentPlayer of
                                    White -> [(1, -1), (1, 1)]
                                    Black -> [(-1, -1), (-1, 1)]
              isOpponentPawn square = case getPiece board square of
                                              Just (Piece player Pawn) -> player == opponentPlayer
                                              _ -> False
              otherPiecesThreaten = False
              kingSquares = map (squareDiff coords) (rookPattern ++ bishopPattern)
              kingsThreaten = any isOpponentKing kingSquares
              isOpponentKing square  = case getPiece board square of
                                                Just (Piece player King) -> player == opponentPlayer
                                                _ -> False
              potentialOpponentRookQueenPieces = catMaybes $ map (firstPieceInSquareList board . iterateDirectionInsideBoard coords) rookPattern
              rookOrQueenThreatens = any isOpponentRookOrQueen potentialOpponentRookQueenPieces
              isOpponentRookOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Rook, Queen]
              potentialOpponentBishopQueenPieces = catMaybes $ map (firstPieceInSquareList board . iterateDirectionInsideBoard coords) bishopPattern
              bishopOrQueenThreatens = any isOpponentBishopOrQueen potentialOpponentBishopQueenPieces
              isOpponentBishopOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Bishop, Queen]
