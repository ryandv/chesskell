module Chess.FenParser
  ( parseFen
  , toFEN
-- these should not be exported! the enPassantSquareParser export is a clever hack.
  , enPassantSquareParser
  , FenParserState(..)
  ) where

import           Chess.Base

import           Control.Monad

import           Data.List

import           Text.Parsec
import           Text.Parsec.Perm
import           Text.Parsec.String

data FenParserState = FenParserState
  { currentLocation :: Coordinate
  }
  deriving Show

parseFen :: SourceName -> String -> Either ParseError RegularGame
parseFen = runParser fenParser (FenParserState $ Coordinate 'a' 8)

fenParser :: GenParser Char FenParserState RegularGame
fenParser = do
  position     <- positionParser
  toMove       <- toMoveParser
  castleRights <- castlingRightsParser
  enPassant    <- enPassantSquareParser
  halfMoves    <- halfMoveClockParser
  fullMoves    <- fullMoveNumberParser
  return $ Game position toMove castleRights enPassant halfMoves fullMoves

positionParser :: GenParser Char FenParserState RegularBoardRepresentation
positionParser = do
  firstSevenRanks <- count 7 rankParser
  eighthRank      <- lastRankParser
  return . reverse $ firstSevenRanks ++ [eighthRank]

rankParser :: GenParser Char FenParserState [Square]
rankParser = do
  squares <- manyTill squareParser $ char '/'
  return $ concat squares

lastRankParser :: GenParser Char FenParserState [Square]
lastRankParser = do
  squares <- manyTill squareParser $ char ' '
  return $ concat squares

nextRank :: FenParserState -> FenParserState
nextRank (FenParserState (Coordinate f r)) =
  FenParserState $ Coordinate f (r + 1)

squareParser :: GenParser Char FenParserState [Square]
squareParser = do
  piece             <- oneOf "rnbqkpRNBQKP12345678"
  currentCoordinate <- fmap currentLocation getState
  case piece of
    'r' -> createSquare currentCoordinate Rook Black
    'n' -> createSquare currentCoordinate Knight Black
    'b' -> createSquare currentCoordinate Bishop Black
    'q' -> createSquare currentCoordinate Queen Black
    'k' -> createSquare currentCoordinate King Black
    'p' -> createSquare currentCoordinate Pawn Black
    'R' -> createSquare currentCoordinate Rook White
    'N' -> createSquare currentCoordinate Knight White
    'B' -> createSquare currentCoordinate Bishop White
    'Q' -> createSquare currentCoordinate Queen White
    'K' -> createSquare currentCoordinate King White
    'P' -> createSquare currentCoordinate Pawn White
    n ->
      let num = read $ return n
      in  replicateM num $ do
            currentCoordinate <- fmap currentLocation getState
            modifyState nextFile
            return (Square Nothing currentCoordinate)
 where
  createSquare
    :: Coordinate
    -> PieceType
    -> Player
    -> GenParser Char FenParserState [Square]
  createSquare c pt ply = modifyState nextFile
    >> (return . return) (Square (Just $ Piece pt ply) c)

nextFile :: FenParserState -> FenParserState
nextFile (FenParserState (Coordinate f r))
  | f == 'h'  = FenParserState $ Coordinate 'a' (r - 1)
  | otherwise = FenParserState $ Coordinate (succ f) r

toMoveParser :: GenParser Char FenParserState Player
toMoveParser = do
  player <- oneOf "wb" >>= (\p -> char ' ' >> return p)
  case player of
    'w' -> return White
    _   -> return Black

castlingRightsParser :: GenParser Char FenParserState CastleRights
castlingRightsParser = do
  castleRights <-
    (char '-' >> return (CastleRights False False False False)) <|> permute
      (    CastleRights
      <$?> (False, char 'K' >> return True)
      <|?> (False, char 'k' >> return True)
      <|?> (False, char 'Q' >> return True)
      <|?> (False, char 'q' >> return True)
      )
  _ <- char ' '
  return castleRights

enPassantSquareParser :: GenParser Char FenParserState (Maybe Coordinate)
enPassantSquareParser =
  fmap Just coordinateParser <|> (char '-' >> char ' ' >> return Nothing) where

  coordinateParser :: GenParser Char FenParserState Coordinate
  coordinateParser = do
    rank <- oneOf "abcdefgh"
    file <- oneOf "12345678"
    _    <- char ' '
    return $ Coordinate rank (read $ return file)

halfMoveClockParser :: GenParser Char FenParserState Int
halfMoveClockParser = do
  halfMoves <- many digit
  _         <- char ' '
  return . read $ halfMoves

fullMoveNumberParser :: GenParser Char FenParserState Int
fullMoveNumberParser = do
  fullMoves <- manyTill digit eof
  return . read $ fullMoves

toFEN :: RegularGame -> String
toFEN Game { placement = position, activeColor = ac, castlingRights = cr, enPassantSquare = eps, halfMoveClock = hmc, fullMoveNumber = fmn }
  = positionString
    ++ toMoveString
    ++ castlingRightsString cr
    ++ enPassantString eps
    ++ halfMovesString
    ++ fullMovesString where
  positionString =
    (++ " ") $ intercalate "/" $ map stringifyRank $ reverse position

  toMoveString | ac == White = "w "
               | otherwise   = "b "

  castlingRightsString (CastleRights True  True  True  True ) = "KQkq "
  castlingRightsString (CastleRights True  True  True  False) = "KQk "
  castlingRightsString (CastleRights True  True  False True ) = "KQq "
  castlingRightsString (CastleRights True  True  False False) = "KQ "
  castlingRightsString (CastleRights True  False True  True ) = "Kkq "
  castlingRightsString (CastleRights True  False True  False) = "Kk "
  castlingRightsString (CastleRights True  False False True ) = "Kq "
  castlingRightsString (CastleRights True  False False False) = "K "
  castlingRightsString (CastleRights False True  True  True ) = "Qkq "
  castlingRightsString (CastleRights False True  True  False) = "Qk "
  castlingRightsString (CastleRights False True  False True ) = "Qq "
  castlingRightsString (CastleRights False True  False False) = "Q "
  castlingRightsString (CastleRights False False True  True ) = "kq "
  castlingRightsString (CastleRights False False True  False) = "k "
  castlingRightsString (CastleRights False False False True ) = "q "
  castlingRightsString (CastleRights False False False False) = "- "

  enPassantString Nothing                 = "- "
  enPassantString (Just (Coordinate f r)) = return f ++ show r ++ " "
  halfMovesString = show hmc ++ " "
  fullMovesString = show fmn

  stringifyRank :: [Square] -> String
  stringifyRank =
    concatMap squishEmptySquares . group . foldr stringifyPiece "" . reverse

  stringifyPiece :: Square -> String -> String
  stringifyPiece (Square (Just (Piece Rook Black)) _) acc = acc ++ "r"
  stringifyPiece (Square (Just (Piece Knight Black)) _) acc = acc ++ "n"
  stringifyPiece (Square (Just (Piece Bishop Black)) _) acc = acc ++ "b"
  stringifyPiece (Square (Just (Piece Queen Black)) _) acc = acc ++ "q"
  stringifyPiece (Square (Just (Piece King Black)) _) acc = acc ++ "k"
  stringifyPiece (Square (Just (Piece Pawn Black)) _) acc = acc ++ "p"
  stringifyPiece (Square (Just (Piece Rook White)) _) acc = acc ++ "R"
  stringifyPiece (Square (Just (Piece Knight White)) _) acc = acc ++ "N"
  stringifyPiece (Square (Just (Piece Bishop White)) _) acc = acc ++ "B"
  stringifyPiece (Square (Just (Piece Queen White)) _) acc = acc ++ "Q"
  stringifyPiece (Square (Just (Piece King White)) _) acc = acc ++ "K"
  stringifyPiece (Square (Just (Piece Pawn White)) _) acc = acc ++ "P"
  stringifyPiece _ acc = acc ++ "-"

  squishEmptySquares :: String -> String
  squishEmptySquares ('-' : xs) = show $ length ('-' : xs)
  squishEmptySquares xs         = xs
