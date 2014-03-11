module Chess.FEN (writeFEN, readFEN) where

import Chess.Internal.Move
import Chess.Internal.FEN

writeFEN :: GameState -> String
writeFEN state = unwords [writeBoard (stateBoard state),
                          writePlayer (currentPlayer state),
                          writeCastlings (whiteCastlingsPossible state) (blackCastlingsPossible state),
                          writeEnPassant (enPassantSquare state),
                          show (halfmoveClock state),
                          show (moveNumber state)]

readFEN :: String -> Maybe GameState
readFEN str | length parts /= 6 = Nothing
            | otherwise = do board' <- readBoard $ head parts
                             player <- readPlayer $ parts !! 1
                             castlings <- readCastlings $ parts !! 2
                             enPassant <- readEnPassant $ parts !! 3
                             halfmoves <- readNumberWithLimit 0 $ parts !! 4
                             moves <- readNumberWithLimit 1 $ parts !! 5
                             return $ uncurry (State board' player) castlings enPassant halfmoves moves
        where parts = words str
