-- |
-- Module      :  Chess
-- Copyright   :  Miika-Petteri Matikainen 2021
-- License     :  GPL-2
--
-- Maintainer  :  miikapetteri@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Visualize chess games
module Chess.UI (
             printBoard
              ) where

import Data.Array
import Data.List
import Data.Char
import Chess.Internal.Board
import Chess.Internal.Piece

-- | Prints the board in ASCII.
--
-- Example for the initial position:
-- @
--
--   +---+---+---+---+---+---+---+---+
-- 8 | r | n | b | q | k | b | n | r |
--   +---+---+---+---+---+---+---+---+
-- 7 | p | p | p | p | p | p | p | p |
--   +---+---+---+---+---+---+---+---+
-- 6 |   |   |   |   |   |   |   |   |
--   +---+---+---+---+---+---+---+---+
-- 5 |   |   |   |   |   |   |   |   |
--   +---+---+---+---+---+---+---+---+
-- 4 |   |   |   |   |   |   |   |   |
--   +---+---+---+---+---+---+---+---+
-- 3 |   |   |   |   |   |   |   |   |
--   +---+---+---+---+---+---+---+---+
-- 2 | P | P | P | P | P | P | P | P |
--   +---+---+---+---+---+---+---+---+
-- 1 | R | N | B | Q | K | B | N | R |
--   +---+---+---+---+---+---+---+---+
--     a   b   c   d   e   f   g   h
--  @
printBoard :: Board -> String
printBoard = addCoordinates . printRows . intoRows . elems
    where intoRows [] = []
          intoRows xs = take 8 xs : intoRows (drop 8 xs)

addCoordinates :: String -> String
addCoordinates str = unlines (zipWith (++) numbers (lines str)) ++ chars
    where numbers = lines $ unlines $ ["  \n" ++ intToDigit n : " " | n <- reverse [1..8]] ++ ["  "]
          chars = "    a   b   c   d   e   f   g   h\n"

printRows :: [[Square]] -> String
printRows rows = line ++ intercalate line (map printRow rows) ++ line
    where line = concat (replicate 8 "+---") ++ "+" ++ "\n"

printRow :: [Square] -> String
printRow row = sep ++ intercalate sep (map printSquare row) ++ sep ++ "\n"
    where sep = "|"

printSquare :: Square -> String
printSquare Empty = "   "
printSquare (Square p) = " " ++ printPiece p ++ " "
