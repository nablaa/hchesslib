module Game (applyMove, isCheckmate) where

import Data.List
import Piece
import Move
import Data.Maybe
import Board

applyMove :: GameState -> Move -> Either MoveError GameState
applyMove game move = case moveError of
                              Just mError -> Left mError
                              Nothing -> Right newGame
        where moveError = isMoveError game move
              newGame = State (updateBoard game move)
                              (updatePlayer game)
                              (updateWhiteCastlings game move)
                              (updateBlackCastlings game move)
                              (updateEnPassantSquare move)
                              (updateHalfMoveClock game move)
                              (updateMoveNumber game)

updateBoard :: GameState -> Move -> Board
updateBoard game move = fromJust $ boardAfterMove (stateBoard game) move

updatePlayer :: GameState -> Color
updatePlayer game = opponent (currentPlayer game)

updateWhiteCastlings :: GameState -> Move -> [CastlingType]
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Movement (Piece White Rook) (7, 0) _) = delete Long castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Movement (Piece White Rook) (7, 7) _) = delete Short castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Capture (Piece White Rook) (7, 0) _) = delete Long castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Capture (Piece White Rook) (7, 7) _) = delete Short castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Capture (Piece Black _) _ (7, 0)) = delete Long castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Capture (Piece Black _) _ (7, 7)) = delete Short castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Promotion (Piece Black _) _ (7, 0) _) = delete Long castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) (Promotion (Piece Black _) _ (7, 7) _) = delete Short castlings
updateWhiteCastlings (State _ _ castlings _ _ _ _) move = updateCastlings White castlings move

updateBlackCastlings :: GameState -> Move -> [CastlingType]
updateBlackCastlings (State _ _ _ castlings _ _ _) (Movement (Piece Black Rook) (0, 0) _) = delete Long castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Movement (Piece Black Rook) (0, 7) _) = delete Short castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Capture (Piece Black Rook) (0, 0) _) = delete Long castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Capture (Piece Black Rook) (0, 7) _) = delete Short castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Capture (Piece White _) _ (0, 0)) = delete Long castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Capture (Piece White _) _ (0, 7)) = delete Short castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Promotion (Piece White _) _ (0, 0) _) = delete Long castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) (Promotion (Piece White _) _ (0, 7) _) = delete Short castlings
updateBlackCastlings (State _ _ _ castlings _ _ _) move = updateCastlings Black castlings move

updateCastlings :: Color -> [CastlingType] -> Move -> [CastlingType]
updateCastlings player _ (Castling movePlayer _) | player == movePlayer = []
updateCastlings player _ (Movement (Piece movePlayer King) _ _) | player == movePlayer = []
updateCastlings player _ (Capture (Piece movePlayer King) _ _) | player == movePlayer = []
updateCastlings _ castlings _ = castlings

updateEnPassantSquare :: Move -> Maybe Coordinates
updateEnPassantSquare (PawnDoubleMove (Piece White Pawn) _ (row, col)) = Just (row + 1, col)
updateEnPassantSquare (PawnDoubleMove (Piece Black Pawn) _ (row, col)) = Just (row - 1, col)
updateEnPassantSquare _ = Nothing

updateHalfMoveClock :: GameState -> Move -> Integer
updateHalfMoveClock _ (Capture _ _ _) = 0
updateHalfMoveClock _ (Movement (Piece _ Pawn) _ _) = 0
updateHalfMoveClock _ (PawnDoubleMove (Piece _ Pawn) _ _) = 0
updateHalfMoveClock _ (Promotion (Piece _ Pawn) _ _ _) = 0
updateHalfMoveClock (State _ _ _ _ _ number _) _ = number + 1

updateMoveNumber :: GameState -> Integer
updateMoveNumber (State _ White _ _ _ _ number) = number
updateMoveNumber (State _ Black _ _ _ _ number) = number + 1

isCheckmate :: GameState -> Bool
isCheckmate game@(State board player _ _ _ _ _) = generateAllMoves game == [] && isCheck board player
