module Piece where

data Piece = Color PieceType
           deriving (Show, Eq)

data Color = White | Black
           deriving (Show, Eq)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
           deriving (Show, Eq)


