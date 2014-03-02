module Move (GameState(..), Move(..), CastlingType(..),
             MoveError(..), isRightPlayerMove,
             isLegalMove, applyMove, initialState,
             isCorrectStartPiece, areCoordinatesValid,
             generateAllRookMoves, iterateMovementSquares,
             iterateDirection, generateAllBishopMoves,
             generateAllQueenMoves, generateAllKnightMoves,
             generateAllKingMoves, generateAllPawnMoves,
             generateAllPotentialMoves, boardAfterMove)  where

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
generateAllQueenMoves game coords = patternMoves game coords queenPattern

generateAllKnightMoves :: GameState -> Coordinates -> [Move]
generateAllKnightMoves game coords = map (\coordinate -> Movement piece coords coordinate) emptySquares
                                     ++ map (\coordinate -> Capture piece coords coordinate) opponentSquares
        where squares = filter isInsideBoard [sumSquares coords jump | jump <- knightPattern]
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
generateAllPawnMoves game coords@(row, _) = move ++ doubleMove ++ captures ++ promotions ++ enpassant
        where board = stateBoard game
              (Piece player _) = fromJust $ getPiece board coords
              isOnStartRow = case player of
                                     White -> row == 6
                                     Black -> row == 1
              isNextToPromotionRow = case player of
                                             White -> row == 1
                                             Black -> row == 6
              moveDirection = case player of
                              White -> -1
                              Black -> 1
              moveSquare = sumSquares coords (moveDirection, 0)
              doubleMoveSquare = sumSquares coords (moveDirection * 2, 0)
              captureSquares = map (sumSquares coords) [(moveDirection, -1), (moveDirection, 1)]
              move = if isEmpty board moveSquare && not isNextToPromotionRow
                     then [Movement (Piece player Pawn) coords moveSquare]
                     else []
              doubleMove = if isEmpty board moveSquare && isEmpty board doubleMoveSquare && isOnStartRow
                           then [PawnDoubleMove (Piece player Pawn) coords doubleMoveSquare]
                           else []
              capture square = if isOpponentSquare board square player && not isNextToPromotionRow
                               then [Capture (Piece player Pawn) coords square]
                               else []
              captures = concatMap capture captureSquares
              promotionCapture square = if isOpponentSquare board square player
                                        then map (Promotion (Piece player Pawn) coords square) [Rook, Bishop, Knight, Queen]
                                        else []
              promotionMove square = if isEmpty board square
                                     then map (Promotion (Piece player Pawn) coords square) [Rook, Bishop, Knight, Queen]
                                     else []
              promotions = if isNextToPromotionRow
                           then concatMap promotionCapture captureSquares ++ promotionMove moveSquare
                           else []
              epSquare = enPassantSquare game
              enpassant = case epSquare of
                                  Just square -> if square `elem` captureSquares
                                                 then [EnPassant (Piece player Pawn) coords square]
                                                 else []
                                  Nothing -> []

kingMoveSquares :: GameState -> Coordinates -> [Coordinates]
kingMoveSquares game start = emptySquares
        where allSquares = map (sumSquares start) queenPattern
              board = stateBoard game
              emptySquares = filter (isEmpty board) $ filter isInsideBoard allSquares

kingCaptureSquares :: GameState -> Coordinates -> [Coordinates]
kingCaptureSquares game start = opponentSquares
        where allSquares = map (sumSquares start) queenPattern
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
              traverseSquaresNotThreatened = not $ any (isSquareThreatened board (opponent player)) (kingSquare : squares)

castlingSquares :: Color -> CastlingType -> [Coordinates]
castlingSquares White Long = [(7, 1), (7, 2), (7, 3)]
castlingSquares White Short = [(7, 5), (7, 6)]
castlingSquares Black Long = [(0, 1), (0, 2), (0, 3)]
castlingSquares Black Short = [(0, 5), (0, 6)]

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

generateAllPotentialMoves :: GameState -> [Move]
generateAllPotentialMoves game = concatMap (generateSquareMoves game) squares
        where player = currentPlayer game
              board = stateBoard game
              squares = getSquaresWithOwner board player

generateSquareMoves :: GameState -> Coordinates -> [Move]
generateSquareMoves game coordinates = case getPiece board coordinates of
                                               Nothing -> []
                                               Just (Piece _ Pawn) -> generateAllPawnMoves game coordinates
                                               Just (Piece _ Rook) -> generateAllRookMoves game coordinates
                                               Just (Piece _ Bishop) -> generateAllBishopMoves game coordinates
                                               Just (Piece _ Queen) -> generateAllQueenMoves game coordinates
                                               Just (Piece _ King) -> generateAllKingMoves game coordinates
                                               Just (Piece _ Knight) -> generateAllKnightMoves game coordinates
        where board = stateBoard game

boardAfterMove :: Board -> Move -> Maybe Board
boardAfterMove board (Movement _ from to) = movePiece from to board
boardAfterMove board (Capture _ from to) = movePiece from to board
boardAfterMove board (PawnDoubleMove _ from to) = movePiece from to board
boardAfterMove board (Castling White Long) = movePiece (7, 0) (7, 3) board >>= movePiece (7, 4) (7, 2)
boardAfterMove board (Castling White Short) = movePiece (7, 7) (7, 5) board >>= movePiece (7, 4) (7, 6)
boardAfterMove board (Castling Black Long) = movePiece (0, 0) (0, 3) board >>= movePiece (0, 4) (0, 2)
boardAfterMove board (Castling Black Short) = movePiece (0, 7) (0, 5) board >>= movePiece (0, 4) (0, 6)
boardAfterMove board (EnPassant (Piece player _) from to@(row, col)) = movePiece from to (removePiece board (epSquare player))
        where epSquare White = (row + 1, col)
              epSquare Black = (row - 1, col)

boardAfterMove board (Promotion (Piece player _) from to promotiontype) =
                case movePiece from to board of
                        Just newboard -> Just $ addPiece newboard to (Piece player promotiontype)
                        Nothing -> Nothing
