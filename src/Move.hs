module Move (GameState(..), Move(..), isLegalMove, applyMove) where

import Piece
import Board

data GameState = State {
      board :: Board
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
          | Promotion Piece Coordinates Coordinates Piece
          | PawnDoubleMove Piece Coordinates Coordinates
          deriving (Show, Eq)

data CastlingType = Short | Long
                  deriving (Show, Eq)

data MoveError = WrongPlayer | InvalidCoordinates
               deriving (Show, Eq)

isLegalMove :: GameState -> Move -> Bool
isLegalMove = undefined

applyMove :: GameState -> Move -> Either MoveError GameState
applyMove = undefined
