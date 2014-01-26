module FEN (writeFEN, readFEN, writeCastlings) where

import Move
import Board
import Piece
import Data.List
import Data.Char
import Data.Maybe

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

readBoard :: String -> Maybe Board
readBoard str | length parts /= 8 = Nothing
              | otherwise = parseBoardCompact $ unlines parts
    where numToEmpty x | isNumber x = replicate (digitToInt x) ' '
                       | otherwise = [x]
          parts = split (== '/') $ concatMap numToEmpty str

readPlayer :: String -> Maybe Color
readPlayer "w" = Just White
readPlayer "b" = Just Black
readPlayer _ = Nothing

readCastlings :: String -> Maybe ([CastlingType], [CastlingType])
readCastlings "-" = Just ([], [])
readCastlings str = case readCastlings' str of
                            Nothing -> Nothing
                            Just (whites, blacks) -> if castlingCountValid whites && castlingCountValid blacks then Just (whites, blacks)
                                                                                                               else Nothing
        where castlingCountValid castlings = sort (nub castlings) == sort castlings

readCastlings' :: String -> Maybe ([CastlingType], [CastlingType])
readCastlings' str = do whiteCastlings <- mapM toCastling whites
                        blackCastlings <- mapM toCastling blacks
                        return (whiteCastlings, blackCastlings)
        where whites = filter isUpper str
              blacks = filter isLower str
              toCastling 'q' = Just Long
              toCastling 'k' = Just Short
              toCastling 'Q' = Just Long
              toCastling 'K' = Just Short
              toCastling _ = Nothing

readEnPassant :: String -> Maybe (Maybe Coordinates)
readEnPassant "-" = Just Nothing
readEnPassant str = case parseCoordinate str of
                      Nothing -> Nothing
                      coordinate -> Just coordinate

readNumberWithLimit :: (Ord a, Read a) => a -> String -> Maybe a
readNumberWithLimit limit str = case readMaybe str of
                                        Nothing -> Nothing
                                        Just number -> if number >= limit
                                                         then Just number
                                                         else Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . filter (null . snd) . reads

split :: (Char -> Bool) -> String -> [String]
split p str = case dropWhile p str of
                "" -> []
                str' -> w : split p str''
                    where (w, str'') = break p str'
