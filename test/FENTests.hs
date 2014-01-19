module FENTests where

import Test.HUnit
import Piece
import Board
import FEN
import Move

writeFENTests :: Test
writeFENTests = TestList [
          "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" ~=? writeFEN initialState
        , "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b - e3 4 14" ~=? writeFEN (Move.State initialBoard Black [] [] (parseCoordinate "e3") 4 14)
        , "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w q c6 0 9" ~=? writeFEN (Move.State initialBoard White [] [Long] (parseCoordinate "c6") 0 9)
        , "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Qk c6 0 9" ~=? writeFEN (Move.State initialBoard White [Long] [Short] (parseCoordinate "c6") 0 9)
        ]

readFENTests :: Test
readFENTests = TestList [
        ]

fenTests :: Test
fenTests = TestList [readFENTests, writeFENTests]
