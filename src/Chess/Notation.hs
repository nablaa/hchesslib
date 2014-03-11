module Chess.Notation (parseMove, parseCoordinateNotation, parseCoordinateStringWithPromotion) where

import Chess.Move
import Chess.Board
import Chess.Piece
import Data.List
import Data.Char
import Data.Attoparsec.Text
import qualified Data.Text as T
import Control.Applicative

parseMove :: GameState -> String -> Maybe Move
parseMove = undefined

parseCoordinateNotation :: GameState -> String -> Maybe Move
parseCoordinateNotation game moveString = case parseOnly parseCoordinateStringWithPromotion (T.pack moveString) of
                                              Left _ -> Nothing
                                              Right (start, end, promotion) -> findMoveForCoordinates game start end promotion

findMoveForCoordinates :: GameState -> Coordinates -> Coordinates -> Maybe PieceType -> Maybe Move
findMoveForCoordinates game start end Nothing | length moves == 1 = Just $ head moves
        where moves = findMovesMatchingCoordinates game start end
findMoveForCoordinates game start end (Just promotion) = findPromotionMove start end promotion moves
        where moves = findMovesMatchingCoordinates game start end
findMoveForCoordinates _ _ _ _ = Nothing

findPromotionMove :: Coordinates -> Coordinates -> PieceType -> [Move] -> Maybe Move
findPromotionMove start end promotion = find matchPromotionMove
        where matchPromotionMove (Promotion _ s e p) = s == start && e == end && p == promotion
              matchPromotionMove _ = False

findMovesMatchingCoordinates :: GameState -> Coordinates -> Coordinates -> [Move]
findMovesMatchingCoordinates game start end = filter (coordinatesMatch start end) allMoves
        where allMoves = generateAllMoves game

coordinatesMatch :: Coordinates -> Coordinates -> Move -> Bool
coordinatesMatch start end (Movement _ from to) = from == start && to == end
coordinatesMatch start end (Capture _ from to) = from == start && to == end
coordinatesMatch start end (EnPassant _ from to) = from == start && to == end
coordinatesMatch start end (PawnDoubleMove _ from to) = from == start && to == end
coordinatesMatch start end (Promotion _ from to _) = from == start && to == end
coordinatesMatch start end (Castling White Short) = start == (7, 4) && end == (7, 6)
coordinatesMatch start end (Castling White Long) = start == (7, 4) && end == (7, 2)
coordinatesMatch start end (Castling Black Short) = start == (0, 4) && end == (0, 6)
coordinatesMatch start end (Castling Black Long) = start == (0, 4) && end == (0, 2)

parseCoordinateStringWithPromotion :: Parser (Coordinates, Coordinates, Maybe PieceType)
parseCoordinateStringWithPromotion = do (coord1, coord2) <- parseCoordinateString
                                        promotion <- parsePromotion
                                        endOfInput
                                        return (coord1, coord2, promotion)

parseCoordinateString :: Parser (Coordinates, Coordinates)
parseCoordinateString = do coord1 <- parseCoordinates
                           _ <- char '-'
                           coord2 <- parseCoordinates
                           return (coord1, coord2)

parseCoordinates :: Parser Coordinates
parseCoordinates = do column <- letter
                      row <- digit
                      case parseCoordinate [toLower column, row] of
                              Just coordinates -> return coordinates
                              Nothing -> fail "Could not parse coordinate"

parsePromotion :: Parser (Maybe PieceType)
parsePromotion = (Just <$> parsePromotionEqualSign) <|> (Just <$> parsePromotionParenthesis) <|> return Nothing

parsePromotionEqualSign :: Parser PieceType
parsePromotionEqualSign = do _ <- char '='
                             promotionChar <- satisfy (`elem` "NBRQ")
                             case parsePieceType promotionChar of
                                     Just piece -> return piece
                                     Nothing -> fail "Invalid promotion piecetype"

parsePromotionParenthesis :: Parser PieceType
parsePromotionParenthesis = do _ <- char '('
                               promotionChar <- satisfy (`elem` "NBRQ")
                               _ <- char ')'
                               case parsePieceType promotionChar of
                                       Just piece -> return piece
                                       Nothing -> fail "Invalid promotion piecetype"
