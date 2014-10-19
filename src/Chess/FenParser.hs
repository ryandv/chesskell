module Chess.FenParser where

import Chess.Base

import Control.Monad

import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String

data FenParserState = FenParserState
  { currentLocation :: Coordinate
  }

parseFen               :: SourceName -> String -> Either ParseError RegularGame
parseFen               = runParser fenParser (FenParserState $ Coordinate 'a' 1)

fenParser              :: GenParser Char FenParserState RegularGame
fenParser              = do
  position <- positionParser
  toMove   <- toMoveParser
  castleRights <- castlingRightsParser
  enPassant <- enPassantSquareParser
  halfMoves <- halfMoveClockParser
  fullMoves <- fullMoveNumberParser
  return $ RegularGame position toMove castleRights enPassant halfMoves fullMoves

positionParser             :: GenParser Char FenParserState RegularBoardRepresentation
positionParser             = do
  firstSevenRanks <- count 7 rankParser
  eighthRank      <- lastRankParser
  return . reverse $ firstSevenRanks ++ [eighthRank]

rankParser             :: GenParser Char FenParserState [Square]
rankParser             = do
  squares <- manyTill squareParser $ char '/'
  modifyState nextRank
  return $ concat squares

lastRankParser             :: GenParser Char FenParserState [Square]
lastRankParser             = do
  squares <- manyTill squareParser $ char ' '
  modifyState nextRank
  return $ concat squares

nextRank                                   :: FenParserState -> FenParserState
nextRank (FenParserState (Coordinate r f)) = FenParserState $ Coordinate (succ r) f

squareParser             :: GenParser Char FenParserState [Square]
squareParser             = do
  piece         <- oneOf "rnbqkpRNBQKP12345678"
  currentSquare <- fmap currentLocation getState
  case piece of
    'r' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Rook Black) currentSquare)
    'n' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Knight Black) currentSquare)
    'b' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Bishop Black) currentSquare)
    'q' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Queen Black) currentSquare)
    'k' -> modifyState nextFile >> (return . return) (Square (Just $ Piece King Black) currentSquare)
    'p' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Pawn Black) currentSquare)
    'R' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Rook White) currentSquare)
    'N' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Knight White) currentSquare)
    'B' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Bishop White) currentSquare)
    'Q' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Queen White) currentSquare)
    'K' -> modifyState nextFile >> (return . return) (Square (Just $ Piece King White) currentSquare)
    'P' -> modifyState nextFile >> (return . return) (Square (Just $ Piece Pawn White) currentSquare)
    n   -> let num = read $ return n in
             replicateM_ num (modifyState nextFile) >> return (replicate (read $ return n) $ Square Nothing currentSquare)

nextFile                                   :: FenParserState -> FenParserState
nextFile (FenParserState (Coordinate r f)) = FenParserState $ Coordinate r (f+1)

toMoveParser             :: GenParser Char FenParserState Player
toMoveParser             = do
  player <- oneOf "wb" >>= (\p -> char ' ' >> return p)
  case player of
    'w' -> return White
    _   -> return Black

castlingRightsParser             :: GenParser Char FenParserState CastleRights
castlingRightsParser             = do
  castleRights <- (char '-' >> return (CastleRights False False False False))
    <|> permute (CastleRights <$?> (False, char 'K' >> return True)
                              <|?> (False, char 'k' >> return True)
                              <|?> (False, char 'Q' >> return True)
                              <|?> (False, char 'q' >> return True))
  char ' '
  return castleRights

enPassantSquareParser             :: GenParser Char FenParserState (Maybe Coordinate)
enPassantSquareParser             = fmap Just coordinateParser <|> (char '-' >> char ' ' >> return Nothing) where

  coordinateParser                :: GenParser Char FenParserState Coordinate
  coordinateParser                = do
    rank <- oneOf "abcdefgh"
    file <- oneOf "12345678"
    char ' '
    return $ Coordinate rank (read $ return file)

halfMoveClockParser             :: GenParser Char FenParserState Integer
halfMoveClockParser             = do
  halfMoves <- many digit
  char ' '
  return . read $ halfMoves

fullMoveNumberParser             :: GenParser Char FenParserState Integer
fullMoveNumberParser             = do
  fullMoves <- manyTill digit eof
  return . read $ fullMoves
