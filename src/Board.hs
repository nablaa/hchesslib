module Board (Board, initialBoard, emptyBoard, printBoardCompact) where

import Data.Array
import Piece

data Square = Square Piece | Empty
              deriving (Eq, Show)

type Coordinates = (Int, Int)
type Board = Array Coordinates Square

initialBoard :: Board
initialBoard = listArray ((0, 0), (7, 7)) rows
    where officerRow color = map (Square . Piece color) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          pawnRow color = replicate 8 $ (Square . Piece color) Pawn
          rows = officerRow Black ++ pawnRow Black ++ replicate 32 Empty ++ pawnRow White ++ officerRow White

emptyBoard :: Board
emptyBoard = listArray ((0, 0), (7, 7)) (repeat Empty)

squareToChar :: Square -> Char
squareToChar Empty = ' '
squareToChar (Square p) = head $ printPiece p

printBoardCompact :: Board -> String
printBoardCompact board = toLines $ foldr f "" (elems board)
    where f = (:) . squareToChar
          toLines [] = []
          toLines str = take 8 str ++ "\n" ++ toLines (drop 8 str)
