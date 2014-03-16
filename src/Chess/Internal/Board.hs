module Chess.Internal.Board (Board, Coordinates, initialBoard, emptyBoard, printBoardCompact,
                    parseCoordinate, isInsideBoard, getPiece, movePiece,
                    parseBoardCompact, printCoordinate, isEmpty, isOpponentSquare,
                    firstPieceInSquareList, iterateDirectionInsideBoard,
                    getKingSquare, rookPattern, bishopPattern, knightPattern,
                    queenPattern, isSquareThreatened, sumSquares,
                    isCheck, getSquaresWithOwner, addPiece, removePiece,
                    getPlayerPieces, getSquareColor, getSquaresWithPieces, Square(..)) where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Chess.Internal.Piece

data Square = Square Piece | Empty
              deriving (Eq, Show)


-- | Coordinate format:
--
-- @
-- (rank, file)
-- (0,0)          (0,7)
--    +---> file
--    |
--    |
--    v
--   rank
--  (7,0)         (7,7)
-- -
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 8 | (0,0) | (0,1) | (0,2) | (0,3) | (0,4) | (0,5) | (0,6) | (0,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 7 | (1,0) | (1,1) | (1,2) | (1,3) | (1,4) | (1,5) | (1,6) | (1,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 6 | (2,0) | (2,1) | (2,2) | (2,3) | (2,4) | (2,5) | (2,6) | (2,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 5 | (3,0) | (3,1) | (3,2) | (3,3) | (3,4) | (3,5) | (3,6) | (3,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 4 | (4,0) | (4,1) | (4,2) | (4,3) | (4,4) | (4,5) | (4,6) | (4,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 3 | (5,0) | (5,1) | (5,2) | (5,3) | (5,4) | (5,5) | (5,6) | (5,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 2 | (6,0) | (6,1) | (6,2) | (6,3) | (6,4) | (6,5) | (6,6) | (6,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
-- 1 | (7,0) | (7,1) | (7,2) | (7,3) | (7,4) | (7,5) | (7,6) | (7,7) |
--   +-------+-------+-------+-------+-------+-------+-------+-------+
--       a       b       c       d       e       f       g       h
--  @
--
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

movePiece :: Coordinates -> Coordinates -> Board -> Maybe Board
movePiece start end _ | not (isInsideBoard start) || not (isInsideBoard end) = Nothing
movePiece start end board = case startPiece of
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

isPlayerSquare :: Board -> Color -> Coordinates -> Bool
isPlayerSquare board player coordinates = case getPiece board coordinates of
                                                  Nothing -> False
                                                  Just (Piece color _) -> color == player

isOpponentSquare :: Board -> Coordinates -> Color -> Bool
isOpponentSquare board coordinates player = isPlayerSquare board (opponent player) coordinates

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

firstPieceInSquareList :: Board -> [Coordinates] -> Maybe Piece
firstPieceInSquareList board coordinates = case firstNonEmpty of
                                                   [] -> Nothing
                                                   (coordinate:_) -> getPiece board coordinate
        where firstNonEmpty = dropWhile (isEmpty board) coordinates

iterateDirectionInsideBoard :: Coordinates -> (Int, Int) -> [Coordinates]
iterateDirectionInsideBoard start direction = tail $ takeWhile isInsideBoard $ iterate (sumSquares direction) start

sumSquares :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumSquares (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getKingSquare :: Board -> Color -> Coordinates
getKingSquare board player = fromJust $ rlookup (Square (Piece player King)) $ assocs board
        where rlookup x = lookup x . map swap
              swap (x, y) = (y, x)

rookPattern :: [(Int, Int)]
rookPattern = [(-1, 0), (1, 0), (0, -1), (0, 1)]

bishopPattern :: [(Int, Int)]
bishopPattern = [(-1, -1), (-1, 1), (1, -1), (1, 1)]

queenPattern :: [(Int, Int)]
queenPattern = rookPattern ++ bishopPattern

knightPattern :: [(Int, Int)]
knightPattern = [(-2, -1), (-1, -2), (1, -2), (2, -1), (-2, 1), (-1, 2), (1, 2), (2, 1)]

isSquareThreatened :: Board -> Color -> Coordinates -> Bool
isSquareThreatened board opponentPlayer coords = knightsThreaten || pawnsThreaten || otherPiecesThreaten || kingsThreaten || rookOrQueenThreatens || bishopOrQueenThreatens
        where knightSquares = map (sumSquares coords) knightPattern
              knightsThreaten = any isOpponentKnight knightSquares
              isOpponentKnight square = case getPiece board square of
                                                Just (Piece player Knight) -> player == opponentPlayer
                                                _ -> False
              pawnsThreaten = any isOpponentPawn $ map (sumSquares coords) pawnSquares
              pawnSquares = case opponentPlayer of
                                    White -> [(1, -1), (1, 1)]
                                    Black -> [(-1, -1), (-1, 1)]
              isOpponentPawn square = case getPiece board square of
                                              Just (Piece player Pawn) -> player == opponentPlayer
                                              _ -> False
              otherPiecesThreaten = False
              kingSquares = map (sumSquares coords) queenPattern
              kingsThreaten = any isOpponentKing kingSquares
              isOpponentKing square  = case getPiece board square of
                                                Just (Piece player King) -> player == opponentPlayer
                                                _ -> False
              potentialOpponentRookQueenPieces = mapMaybe (firstPieceInSquareList board . iterateDirectionInsideBoard coords) rookPattern
              rookOrQueenThreatens = any isOpponentRookOrQueen potentialOpponentRookQueenPieces
              isOpponentRookOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Rook, Queen]
              potentialOpponentBishopQueenPieces = mapMaybe (firstPieceInSquareList board . iterateDirectionInsideBoard coords) bishopPattern
              bishopOrQueenThreatens = any isOpponentBishopOrQueen potentialOpponentBishopQueenPieces
              isOpponentBishopOrQueen (Piece color piecetype) = color == opponentPlayer && piecetype `elem` [Bishop, Queen]

isCheck :: Board -> Color -> Bool
isCheck board player = isSquareThreatened board (opponent player) kingSquare
        where kingSquare = getKingSquare board player

getSquaresWithOwner :: Board -> Color -> [Coordinates]
getSquaresWithOwner board player = filter (isPlayerSquare board player) (indices board)

getSquaresWithPieces :: Board -> PieceType -> [Coordinates]
getSquaresWithPieces board piecetype = filter (isPieceSquare board piecetype) (indices board)

isPieceSquare :: Board -> PieceType -> Coordinates -> Bool
isPieceSquare board piecetype square = case getPiece board square of
                                               Just (Piece _ t) -> t == piecetype
                                               Nothing -> False

getPlayerPieces :: Board -> Color -> [PieceType]
getPlayerPieces board player = map (typeFromPiece . fromJust . getPiece board) (getSquaresWithOwner board player)
        where typeFromPiece (Piece _ pieceType) = pieceType

getSquareColor :: Coordinates -> Color
getSquareColor (row, column) | (row + column) `mod` 2 == 0 = White
                             | otherwise = Black
