{-# LANGUAGE OverloadedStrings #-}
module Chess.FastFenParser
  ( fastParseFEN
  , fastToFEN
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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString.Lazy as P
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.Internal.Types as I

import           Text.Parser.Permutation

data FenParserState = FenParserState
  { currentLocation :: Coordinate
  }
  deriving Show

fastParseFEN :: BSL.ByteString -> Either String (Game BitboardRepresentation)
fastParseFEN = P.eitherResult . P.parse (evalStateT fenParser (FenParserState $ Coordinate 'a' 8))

fenParser :: StateT FenParserState P.Parser (Game BitboardRepresentation)
fenParser = do
  position     <- positionParser
  toMove       <- toMoveParser
  castleRights <- castlingRightsParser
  enPassant    <- enPassantSquareParser
  halfMoves    <- halfMoveClockParser
  fullMoves    <- fullMoveNumberParser
  return $ Game position toMove castleRights enPassant halfMoves fullMoves

positionParser :: StateT FenParserState P.Parser BitboardRepresentation
positionParser = do
  firstSevenRanks <- count 7 rankParser
  eighthRank      <- lastRankParser
  let finalBitboard = foldr uniteRepresentation emptyBitboardRepresentation . concat $ firstSevenRanks ++ [eighthRank]
  let whiteOccupancy = (whitePawns finalBitboard `bitboardUnion` whiteKnights finalBitboard `bitboardUnion` whiteBishops finalBitboard `bitboardUnion` whiteRooks finalBitboard `bitboardUnion` whiteQueens finalBitboard `bitboardUnion` whiteKings finalBitboard)
  let blackOccupancy = (blackPawns finalBitboard `bitboardUnion` blackKnights finalBitboard `bitboardUnion` blackBishops finalBitboard `bitboardUnion` blackRooks finalBitboard `bitboardUnion` blackQueens finalBitboard `bitboardUnion` blackKings finalBitboard)
  return $ finalBitboard
    { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
    , whiteOccupancy = whiteOccupancy
    , blackOccupancy = blackOccupancy
    }

rankParser :: StateT FenParserState P.Parser [(Maybe Piece, Coordinate)]
rankParser = fmap concat . manyTill squareParser . lift $ char '/'

lastRankParser :: StateT FenParserState P.Parser [(Maybe Piece, Coordinate)]
lastRankParser = fmap concat . manyTill squareParser . lift $ char ' '

uniteRepresentation :: (Maybe Piece, Coordinate) -> BitboardRepresentation -> BitboardRepresentation
uniteRepresentation (piece, c) bb = addPieceTo bb piece c

nextRank :: FenParserState -> FenParserState
nextRank (FenParserState (Coordinate f r)) =
  FenParserState $ Coordinate f (r + 1)

squareParser :: StateT FenParserState Parser [(Maybe Piece, Coordinate)]
squareParser = do
  piece             <- lift $ satisfy (`BS.elem` ("rnbqkpRNBQKP12345678" :: BS.ByteString))
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

toMoveParser :: StateT FenParserState P.Parser Player
toMoveParser = do
  player <- lift $ satisfy (`BS.elem` "wb") >>= ((char ' ' >>) . return)
  case player of
    'w' -> return White
    _   -> return Black

castlingRightsParser :: StateT FenParserState P.Parser CastleRights
castlingRightsParser = do
  castleRights <-
    lift ((char '-' >> return (CastleRights False False False False)) <|> permute permutationParser)
  _ <- lift $ char ' '
  return castleRights where

  permutationParser = CastleRights
    <$?> (False, char 'K' >> return True)
    <|?> (False, char 'k' >> return True)
    <|?> (False, char 'Q' >> return True)
    <|?> (False, char 'q' >> return True)

enPassantSquareParser :: StateT FenParserState P.Parser (Maybe Coordinate)
enPassantSquareParser =
  fmap Just coordinateParser <|> (lift (char '-') >> lift (char ' ') >> return Nothing) where

  coordinateParser :: StateT FenParserState P.Parser Coordinate
  coordinateParser = do
    rank <- lift $ satisfy (`BS.elem` "abcdefgh")
    file <- lift $ satisfy (`BS.elem` "12345678")
    _    <- lift $ char ' '
    return $ Coordinate rank (digitToInt file)

halfMoveClockParser :: StateT FenParserState P.Parser Int
halfMoveClockParser = do
  halfMoves <- lift decimal
  _         <- lift $ char ' '
  return halfMoves

fullMoveNumberParser :: StateT FenParserState P.Parser Int
fullMoveNumberParser = lift decimal

fastToFEN :: (Game BitboardRepresentation) -> String
fastToFEN Game { placement = position, activeColor = ac, castlingRights = cr, enPassantSquare = eps, halfMoveClock = hmc, fullMoveNumber = fmn }
  = positionString
    ++ toMoveString
    ++ castlingRightsString cr
    ++ enPassantString eps
    ++ halfMovesString
    ++ fullMovesString where
  positionString =
    (++ " ") $ intercalate "/" $ map (stringifyRank position) $ (map . map) (indicesToCoordinate . squareIndexToIndices) $ reverse
    [[0..7], [8..15], [16..23], [24..31], [32..39], [40..47], [48..55], [56..63]]

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

  stringifyRank :: BitboardRepresentation -> [Coordinate] -> String
  stringifyRank bb =
    concatMap squishEmptySquares . group . foldr (stringifyPiece . bitboardPieceAt bb) "" . reverse

  stringifyPiece :: Maybe Piece -> String -> String
  stringifyPiece (Just (Piece Rook Black)) acc = acc ++ "r"
  stringifyPiece (Just (Piece Knight Black)) acc = acc ++ "n"
  stringifyPiece (Just (Piece Bishop Black)) acc = acc ++ "b"
  stringifyPiece (Just (Piece Queen Black)) acc = acc ++ "q"
  stringifyPiece (Just (Piece King Black)) acc = acc ++ "k"
  stringifyPiece (Just (Piece Pawn Black)) acc = acc ++ "p"
  stringifyPiece (Just (Piece Rook White)) acc = acc ++ "R"
  stringifyPiece (Just (Piece Knight White)) acc = acc ++ "N"
  stringifyPiece (Just (Piece Bishop White)) acc = acc ++ "B"
  stringifyPiece (Just (Piece Queen White)) acc = acc ++ "Q"
  stringifyPiece (Just (Piece King White)) acc = acc ++ "K"
  stringifyPiece (Just (Piece Pawn White)) acc = acc ++ "P"
  stringifyPiece _ acc = acc ++ "-"

  squishEmptySquares :: String -> String
  squishEmptySquares ('-' : xs) = show $ length ('-' : xs)
  squishEmptySquares xs         = xs
