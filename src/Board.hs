module Board (Board, Coordinates, initialBoard, emptyBoard, printBoardCompact,
              parseCoordinate, isInsideBoard) where

import Data.Array
import Data.Char
import Piece

data Square = Square Piece | Empty
              deriving (Eq, Show)

{-
(0,0)          (0,7)
   +---> column
   |
   |
   v
  row

 (7,0)         (7,7)
-}
type Coordinates = (Int, Int) -- | (row, column)
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

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7

parseCoordinate :: String -> Maybe Coordinates
parseCoordinate (column:row:[]) | isInsideBoard coordinates = Just coordinates
                                    | otherwise = Nothing
    where coordinates = (ord '8' - ord row, ord column - ord 'a')
parseCoordinate _ = Nothing
