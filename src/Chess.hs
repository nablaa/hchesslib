-- |
-- Module      :  Chess
-- Copyright   :  Miika-Petteri Matikainen 2014
-- License     :  GPL-2
--
-- Maintainer  :  miikapetteri@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple chess library for implementing chess games.
module Chess (
              Chess.Internal.Board.Board,
              Chess.Internal.Board.Coordinates,
              Chess.Internal.Board.Square(..),
              Chess.Internal.Move.GameState,
              Chess.Internal.Move.currentPlayer,
              Chess.Internal.Piece.Color(..),
              Chess.Internal.Piece.Piece(..),
              Chess.Internal.Piece.PieceType(..),
              board,
              fullMoveNumber,
              isCheckmate,
              isDraw,
              isLegalMove,
              isStalemate,
              move,
              newGame,
              pieceAt,
              winner,
              ) where

import Chess.Internal.Board
import Chess.Internal.Move
import Chess.Internal.Piece
import qualified Chess.Internal.Game as G
import qualified Chess.Internal.Notation as N

-- | Has the game ended in checkmate
isCheckmate :: GameState -> Bool
isCheckmate = G.isCheckmate

-- | Has the game ended in stalemate
isStalemate :: GameState -> Bool
isStalemate = G.isStalemate

-- | Is the game draw? I.e. is the game stalemate or is the game draw by
-- insufficient material.
isDraw :: GameState -> Bool
isDraw = G.isDraw

-- | Returns the winner of the game if any
winner :: GameState -> Maybe Color
winner = G.getWinner

-- | Is the given move legal. The only supported move format at the moment
-- is coordinate notation.
isLegalMove :: GameState
            -> String    -- ^ Move in coordinate notation. E.g. "e2-e4" or "b1-c3"
            -> Bool
isLegalMove game moveStr = case N.parseMove game moveStr of
                                   Just m -> m `elem` generateAllMoves game
                                   Nothing -> False

-- | Make a move. The only supported move format at the moment is coordinate
-- notation.
move :: GameState
     -> String          -- ^ Move in coordinate notation. E.g. "e2-e4" or "b1-c3"
     -> Maybe GameState
move game moveStr = do m <- N.parseMove game moveStr
                       case G.applyMove game m of
                               Left _ -> Nothing
                               Right game' -> Just game'

-- | Current board state in the game
board :: GameState -> Board
board = stateBoard

-- | Get initial game state
newGame :: GameState
newGame = initialState

-- | Get the piece at the given coordinate
pieceAt :: Board
        -> String      -- ^ Square coordinate. E.g. "e4"
        -> Maybe Piece
pieceAt b coordinateStr = do coords <- parseCoordinate coordinateStr
                             getPiece b coords

-- | Full move number. Incremented after black's move.
fullMoveNumber :: GameState -> Integer
fullMoveNumber = moveNumber
