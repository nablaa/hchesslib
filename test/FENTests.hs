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
          Just initialState ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        , Just (Move.State initialBoard White [Short] [Long] (parseCoordinate "e3") 7 14) ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w Kq e3 7 14"
        , Just (Move.State initialBoard White [] [] Nothing 10 42) ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 10 42"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR K KQkq - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - a 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 a"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/7/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        , Nothing ~=? readFEN "nbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KKkq - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w qKqQ - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkK - 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - -1 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 0"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 -1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq a9 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq i8 0 1"
        , Nothing ~=? readFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq 44 0 1"
        , Nothing ~=? readFEN "foobar"
        , Nothing ~=? readFEN ""
        , Nothing ~=? readFEN "1 2 3 4 5 6"
        ]

fenTests :: Test
fenTests = TestList [readFENTests, writeFENTests]
