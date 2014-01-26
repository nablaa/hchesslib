module Move (GameState(..), Move(..), CastlingType(..),
             MoveError(..), isRightPlayerMove,
             isLegalMove, applyMove, initialState,
             isCorrectStartPiece) where

import Piece
import Board

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
          deriving (Show, Eq)

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

