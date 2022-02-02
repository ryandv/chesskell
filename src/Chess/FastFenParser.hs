{-# LANGUAGE OverloadedStrings #-}
module Chess.FastFenParser
  ( fastParseFEN
  , toFEN
  , FenParserState(..)
  ) where

import           Chess.Base
import           Chess.Bitboard

import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Applicative

import           Data.Char
import           Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Internal as BSI
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Internal.Types as I

import           Text.Parser.Permutation

data FenParserState = FenParserState
  { currentLocation :: Coordinate
  }
  deriving Show

fastParseFEN :: BSI.ByteString -> Either String (Game BitboardRepresentation)
fastParseFEN = parseOnly (evalStateT fenParser (FenParserState $ Coordinate 'a' 8))

fenParser :: StateT FenParserState Parser (Game BitboardRepresentation)
fenParser = do
  position     <- positionParser
  toMove       <- toMoveParser
  castleRights <- castlingRightsParser
  enPassant    <- enPassantSquareParser
  halfMoves    <- halfMoveClockParser
  fullMoves    <- fullMoveNumberParser
  return $ Game position toMove castleRights enPassant halfMoves fullMoves

positionParser :: StateT FenParserState Parser BitboardRepresentation
positionParser = do
  firstSevenRanks <- count 7 rankParser
  eighthRank      <- lastRankParser
  let finalBitboard = foldr uniteRepresentation emptyBitboardRepresentation . concat $ firstSevenRanks ++ [eighthRank]
  return $ finalBitboard
    { totalOccupancy = whitePawns finalBitboard
      `bitboardUnion` whiteKnights finalBitboard
      `bitboardUnion` whiteBishops finalBitboard
      `bitboardUnion` whiteRooks finalBitboard
      `bitboardUnion` whiteQueens finalBitboard
      `bitboardUnion` whiteKings finalBitboard
      `bitboardUnion` blackPawns finalBitboard
      `bitboardUnion` blackBishops finalBitboard
      `bitboardUnion` blackKnights finalBitboard
      `bitboardUnion` blackRooks finalBitboard
      `bitboardUnion` blackQueens finalBitboard
      `bitboardUnion` blackKings finalBitboard
    }

rankParser :: StateT FenParserState Parser [(Maybe Piece, Coordinate)]
rankParser = fmap concat . manyTill squareParser . lift $ char '/'

lastRankParser :: StateT FenParserState Parser [(Maybe Piece, Coordinate)]
lastRankParser = fmap concat . manyTill squareParser . lift $ char ' '

uniteRepresentation :: (Maybe Piece, Coordinate) -> BitboardRepresentation -> BitboardRepresentation
uniteRepresentation (piece, c) bb = addPieceTo bb piece c

nextRank :: FenParserState -> FenParserState
nextRank (FenParserState (Coordinate f r)) =
  FenParserState $ Coordinate f (r + 1)

squareParser :: StateT FenParserState Parser [(Maybe Piece, Coordinate)]
squareParser = do
  piece             <- lift $ satisfy (`BS.elem` ("rnbqkpRNBQKP12345678" :: BSI.ByteString))
  currentCoordinate <- fmap currentLocation get
  case piece of
    'r' -> modify nextFile >> ((return . return) $ ((Just $ Piece Rook Black), currentCoordinate))
    'n' -> modify nextFile >> ((return . return) $ ((Just $ Piece Knight Black), currentCoordinate))
    'b' -> modify nextFile >> ((return . return) $ ((Just $ Piece Bishop Black), currentCoordinate))
    'q' -> modify nextFile >> ((return . return) $ ((Just $ Piece Queen Black), currentCoordinate))
    'k' -> modify nextFile >> ((return . return) $ ((Just $ Piece King Black), currentCoordinate))
    'p' -> modify nextFile >> ((return . return) $ ((Just $ Piece Pawn Black), currentCoordinate))
    'R' -> modify nextFile >> ((return . return) $ ((Just $ Piece Rook White), currentCoordinate))
    'N' -> modify nextFile >> ((return . return) $ ((Just $ Piece Knight White), currentCoordinate))
    'B' -> modify nextFile >> ((return . return) $ ((Just $ Piece Bishop White), currentCoordinate))
    'Q' -> modify nextFile >> ((return . return) $ ((Just $ Piece Queen White), currentCoordinate))
    'K' -> modify nextFile >> ((return . return) $ ((Just $ Piece King White), currentCoordinate))
    'P' -> modify nextFile >> ((return . return) $ ((Just $ Piece Pawn White), currentCoordinate))
    n ->
      let num = digitToInt n
      in  replicateM num $ do
            currentCoordinate <- fmap currentLocation get
            modify nextFile
            return (Nothing, currentCoordinate)

nextFile :: FenParserState -> FenParserState
nextFile (FenParserState (Coordinate f r))
  | f == 'h'  = FenParserState $ Coordinate 'a' (r - 1)
  | otherwise = FenParserState $ Coordinate (succ f) r

toMoveParser :: StateT FenParserState Parser Player
toMoveParser = do
  player <- lift $ satisfy (`BS.elem` "wb") >>= ((char ' ' >>) . return)
  case player of
    'w' -> return White
    _   -> return Black

castlingRightsParser :: StateT FenParserState Parser CastleRights
castlingRightsParser = do
  castleRights <-
    lift ((char '-' >> return (CastleRights False False False False)) <|> permute permutationParser)
  _ <- lift $ char ' '
  return castleRights where

  permutationParser :: Permutation (I.Parser BSI.ByteString) CastleRights
  permutationParser = CastleRights
    <$?> (False, char 'K' >> return True)
    <|?> (False, char 'k' >> return True)
    <|?> (False, char 'Q' >> return True)
    <|?> (False, char 'q' >> return True)

enPassantSquareParser :: StateT FenParserState Parser (Maybe Coordinate)
enPassantSquareParser =
  fmap Just coordinateParser <|> (lift (char '-') >> lift (char ' ') >> return Nothing) where

  coordinateParser :: StateT FenParserState Parser Coordinate
  coordinateParser = do
    rank <- lift $ satisfy (`BS.elem` "abcdefgh")
    file <- lift $ satisfy (`BS.elem` "12345678")
    _    <- lift $ char ' '
    return $ Coordinate rank (digitToInt file)

halfMoveClockParser :: StateT FenParserState Parser Int
halfMoveClockParser = do
  halfMoves <- lift decimal
  _         <- lift $ char ' '
  return halfMoves

fullMoveNumberParser :: StateT FenParserState Parser Int
fullMoveNumberParser = lift decimal

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
