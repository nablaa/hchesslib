module TestUtils (coord, game, fenBoard) where

import Chess.Internal.Board
import Chess.Internal.Move
import Chess.FEN
import Data.Maybe

coord :: String -> Coordinates
coord = fromJust . parseCoordinate

game :: String -> GameState
game = fromJust . readFEN

fenBoard :: String -> Board
fenBoard = stateBoard . fromJust . readFEN
