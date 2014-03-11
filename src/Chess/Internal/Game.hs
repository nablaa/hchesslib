module Chess.Internal.Game (applyMove, isCheckmate, isStalemate, isInsufficientMaterial,
                   isDraw, getWinner) where

import Data.List
import Chess.Internal.Piece
import Chess.Internal.Move
import Chess.Internal.Board
import Data.Maybe

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
updateHalfMoveClock _ Capture{} = 0
updateHalfMoveClock _ (Movement (Piece _ Pawn) _ _) = 0
updateHalfMoveClock _ (PawnDoubleMove (Piece _ Pawn) _ _) = 0
updateHalfMoveClock _ (Promotion (Piece _ Pawn) _ _ _) = 0
updateHalfMoveClock (State _ _ _ _ _ number _) _ = number + 1

updateMoveNumber :: GameState -> Integer
updateMoveNumber (State _ White _ _ _ _ number) = number
updateMoveNumber (State _ Black _ _ _ _ number) = number + 1

isCheckmate :: GameState -> Bool
isCheckmate game@(State board player _ _ _ _ _) = null (generateAllMoves game) && isCheck board player

isStalemate :: GameState -> Bool
isStalemate game@(State board player _ _ _ _ _) = null (generateAllMoves game) && not (isCheck board player)

isInsufficientMaterial :: GameState -> Bool
isInsufficientMaterial game = isInsufficientMaterialByPieces whitePieces blackPieces || isInsufficientMaterialWithBishops board whitePieces blackPieces
        where board = stateBoard game
              whitePieces = delete King $ getPlayerPieces board White
              blackPieces = delete King $ getPlayerPieces board Black

isInsufficientMaterialByPieces :: [PieceType] -> [PieceType] -> Bool
isInsufficientMaterialByPieces [] [] = True
isInsufficientMaterialByPieces [Bishop] [] = True
isInsufficientMaterialByPieces [Knight] [] = True
isInsufficientMaterialByPieces [] [Bishop] = True
isInsufficientMaterialByPieces [] [Knight] = True
isInsufficientMaterialByPieces _ _ = False

isInsufficientMaterialWithBishops :: Board -> [PieceType] -> [PieceType] -> Bool
isInsufficientMaterialWithBishops _ white black | not (onlyBishops white && onlyBishops black) = False
        where onlyBishops pieces = not (any (/= Bishop) pieces)
isInsufficientMaterialWithBishops board _ _ = bishopsOnWhite /= bishopsOnBlack
        where whiteSquaresWithBishops = filter (\x -> getSquareColor x == White) $ getSquaresWithPieces board Bishop
              blackSquaresWithBishops = filter (\x -> getSquareColor x == Black) $ getSquaresWithPieces board Bishop
              bishopsOnWhite = not (null whiteSquaresWithBishops)
              bishopsOnBlack = not (null blackSquaresWithBishops)

isDraw :: GameState -> Bool
isDraw game = isStalemate game || isInsufficientMaterial game

getWinner :: GameState -> Maybe Color
getWinner game | isCheckmate game = Just $ opponent $ currentPlayer game
               | otherwise = Nothing
