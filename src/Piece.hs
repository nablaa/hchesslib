module Piece (Piece(..), Color(..), PieceType(..), opponent, printPiece,
              parsePiece) where

import Data.Char

data Piece = Piece Color PieceType
           deriving (Show, Eq)

data Color = White | Black
           deriving (Show, Eq)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
           deriving (Show, Eq)

opponent :: Color -> Color
opponent White = Black
opponent Black = White

pieceChars :: [(PieceType, Char)]
pieceChars = [(Pawn, 'P'), (Knight, 'N'), (Bishop, 'B'), (Rook, 'R'), (Queen, 'Q'), (King, 'K')]

printPiece :: Piece -> String
printPiece (Piece color pieceType) = case color of
                               White -> [toUpper c]
                               Black -> [toLower c]
    where (Just c) = lookup pieceType pieceChars

parsePiece :: Char -> Maybe Piece
parsePiece c = do
  pieceType <- rlookup (toUpper c) pieceChars
  return (if isUpper c then Piece White pieceType else Piece Black pieceType)
    where rlookup x = lookup x . map swap
          swap (x, y) = (y, x)
