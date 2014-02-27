module TestUtils (coord, game, fenBoard) where

import Board
import Move
import FEN
import Data.Maybe

coord :: String -> Coordinates
coord = fromJust . parseCoordinate

game :: String -> GameState
game = fromJust . readFEN

fenBoard :: String -> Board
fenBoard = stateBoard . fromJust . readFEN
