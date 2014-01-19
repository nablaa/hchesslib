module FEN (writeFEN, readFEN, writeCastlings) where

import Move
import Board
import Piece
import Data.List

writeBoard :: Board -> String
writeBoard = intercalate "/" . lines . concatMap emptyToNum . group . printBoardCompact
    where emptyToNum str@(' ':_) = show $ length str
          emptyToNum str = str

writePlayer :: Color -> String
writePlayer White = "w"
writePlayer Black = "b"

writeCastlings :: [CastlingType] -> [CastlingType] -> String
writeCastlings [] [] = "-"
writeCastlings white black = sort (map (castlingToChar White) white ++ map (castlingToChar Black) black)
        where castlingToChar White Long = 'Q'
              castlingToChar White Short = 'K'
              castlingToChar Black Long = 'q'
              castlingToChar Black Short = 'k'

writeEnPassant :: Maybe Coordinates -> String
writeEnPassant Nothing = "-"
writeEnPassant (Just coordinate) = printCoordinate coordinate

writeFEN :: GameState -> String
writeFEN state = unwords [writeBoard (board state),
                          writePlayer (currentPlayer state),
                          writeCastlings (whiteCastlingsPossible state) (blackCastlingsPossible state),
                          writeEnPassant (enPassantSquare state),
                          show (halfmoveClock state),
                          show (moveNumber state)]

readFEN :: String -> Maybe GameState
readFEN = undefined
