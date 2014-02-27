module TestUtils (coord, game) where

import Board
import Move
import FEN
import Data.Maybe

coord :: String -> Coordinates
coord = fromJust . parseCoordinate

game :: String -> GameState
game = fromJust . readFEN
