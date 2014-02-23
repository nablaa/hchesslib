module Board (Board, Coordinates, initialBoard, emptyBoard, printBoardCompact,
              parseCoordinate, isInsideBoard, getPiece, movePiece,
              parseBoardCompact, printCoordinate, isEmpty, isOpponentSquare) where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Piece

data Square = Square Piece | Empty
              deriving (Eq, Show)

{- | (row, column)
@
(0,0)          (0,7)
   +---> column
   |
   |
   v
  row

 (7,0)         (7,7)
@
-}
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

isInsideBoard :: Coordinates -> Bool
isInsideBoard (i, j) = i >= 0 && i <= 7 && j >= 0 && j <= 7

parseCoordinate :: String -> Maybe Coordinates
parseCoordinate (column:row:[]) | isInsideBoard coordinates = Just coordinates
                                    | otherwise = Nothing
    where coordinates = (ord '8' - ord row, ord column - ord 'a')
parseCoordinate _ = Nothing

printCoordinate :: Coordinates -> String
printCoordinate (r, c) = [chr (ord 'a' + c), intToDigit (8 - r)]

movePiece :: Board -> Coordinates -> Coordinates -> Maybe Board
movePiece _ start end | not (isInsideBoard start) || not (isInsideBoard end) = Nothing
movePiece board start end = case startPiece of
                                    Nothing -> Nothing
                                    Just piece -> Just $ addPiece board' end piece
        where startPiece = getPiece board start
              board' = removePiece board start

addPiece :: Board -> Coordinates -> Piece -> Board
addPiece board coordinates = updateBoard board coordinates . Square

removePiece :: Board -> Coordinates -> Board
removePiece board coordinates = updateBoard board coordinates Empty

updateBoard :: Board -> Coordinates -> Square -> Board
updateBoard board coordinates square = board // [(coordinates, square)]

getPiece :: Board -> Coordinates -> Maybe Piece
getPiece board coordinates | inRange (bounds board) coordinates = f $ board ! coordinates
                           where f Empty = Nothing
                                 f (Square piece) = Just piece
getPiece _ _ = Nothing

isEmpty :: Board -> Coordinates -> Bool
isEmpty board coordinates = isNothing $ getPiece board coordinates

isOpponentSquare :: Board -> Coordinates -> Color -> Bool
isOpponentSquare board coordinates player = case getPiece board coordinates of
                                                    Nothing -> False
                                                    Just (Piece color _) -> color == opponent player

parseBoardCompact :: String -> Maybe Board
parseBoardCompact str | length str /= 72 = Nothing
                      | length rows /= 8 || nub (map length rows) /= [8] = Nothing
                      | otherwise = squares >>= boardFromSquares
        where rows = lines str
              squares = mapM parseSquare (concat rows)

parseSquare :: Char -> Maybe Square
parseSquare ' ' = Just Empty
parseSquare c = case parsePiece c of
                        Nothing -> Nothing
                        Just piece -> Just $ Square piece

boardFromSquares :: [Square] -> Maybe Board
boardFromSquares squares | length squares /= 64 = Nothing
                         | otherwise = Just $ listArray ((0, 0), (7, 7)) squares
