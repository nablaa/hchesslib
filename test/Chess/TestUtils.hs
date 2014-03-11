module TestUtils (coord, game, fenBoard) where

import Chess.Board
import Chess.Move
import Chess.FEN
import Data.Maybe

coord :: String -> Coordinates
coord = fromJust . parseCoordinate

game :: String -> GameState
game = fromJust . readFEN

fenBoard :: String -> Board
fenBoard = stateBoard . fromJust . readFEN
