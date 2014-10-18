module Chess where

import Control.Monad

import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String

data Square = Square
  { pieceOn  :: Maybe Piece
  , location :: Coordinate
  } deriving(Read, Show)

data Piece = Piece
  { pieceType  :: PieceType
  , pieceOwner :: Player
  } deriving(Read)

instance Show Piece where
  show (Piece Rook White)   = "R"
  show (Piece Knight White) = "N"
  show (Piece Bishop White) = "B"
  show (Piece Queen White)  = "Q"
  show (Piece King White)   = "K"
  show (Piece Pawn White)   = "P"

  show (Piece Rook Black)   = "r"
  show (Piece Knight Black) = "n"
  show (Piece Bishop Black) = "b"
  show (Piece Queen Black)  = "q"
  show (Piece King Black)   = "k"
  show (Piece Pawn Black)   = "p"

data PieceType  = Pawn | Knight | Bishop | Rook | Queen | King deriving (Enum, Eq, Read)
data Player = White | Black deriving (Enum, Eq, Read, Show)

data Coordinate = Coordinate Rank File deriving(Read, Show)
type Rank   = Char
type File   = Integer

-- KkQq
data CastleRights = CastleRights Bool Bool Bool Bool deriving(Show)

type RegularBoardRepresentation   = [[Square]]

data RegularGame = RegularGame
  { placement       :: RegularBoardRepresentation
  , activeColor     :: Player
  , castlingRights  :: CastleRights
  , enPassantSquare :: Maybe Coordinate
  , halfMoveClock   :: Integer
  , fullMoveNumber  :: Integer
  }

instance Show RegularGame where
  show g =    "\n"
           ++ "  abcdefgh \n"
           ++ "8 " ++ concatMap (maybe "-" show) eighthRank  ++ " 8\n"
           ++ "7 " ++ concatMap (maybe "-" show) seventhRank ++ " 7\n"
           ++ "6 " ++ concatMap (maybe "-" show) sixthRank   ++ " 6\n"
           ++ "5 " ++ concatMap (maybe "-" show) fifthRank   ++ " 5\n"
           ++ "4 " ++ concatMap (maybe "-" show) fourthRank  ++ " 4\n"
           ++ "3 " ++ concatMap (maybe "-" show) thirdRank   ++ " 3\n"
           ++ "2 " ++ concatMap (maybe "-" show) secondRank  ++ " 2\n"
           ++ "1 " ++ concatMap (maybe "-" show) firstRank   ++ " 1\n"
           ++ "  abcdefgh \n " where
    eighthRank  = map pieceOn $ placement g !! 7
    seventhRank = map pieceOn $ placement g !! 6
    sixthRank   = map pieceOn $ placement g !! 5
    fifthRank   = map pieceOn $ placement g !! 4
    fourthRank  = map pieceOn $ placement g !! 3
    thirdRank   = map pieceOn $ placement g !! 2
    secondRank  = map pieceOn $ placement g !! 1
    firstRank   = map pieceOn $ placement g !! 0

pseudoLegalMoves                   :: RegularGame -> [(Coordinate, Coordinate)]
pseudoLegalMoves RegularGame { placement = b
                             , enPassantSquare = e
                             } = (concatMap . concatMap) (pseudoLegalMovesFrom e) b where

pseudoLegalMovesFrom :: Maybe Coordinate -> Square -> [(Coordinate, Coordinate)]
pseudoLegalMovesFrom _ (Square Nothing _)            = []
pseudoLegalMovesFrom c (Square (Just (Piece p _)) l) | p == Pawn   = pseudoLegalPawnMoves c l
                                                     | p == Knight = pseudoLegalKnightMoves l
                                                     | p == Bishop = pseudoLegalBishopMoves l
                                                     | p == Rook   = pseudoLegalRookMoves l
                                                     | p == Queen  = pseudoLegalQueenMoves l
                                                     | p == King   = pseudoLegalKingMoves l


pseudoLegalPawnMoves                          :: Maybe Coordinate -> Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalPawnMoves Nothing c                = standardPawnMoves c
pseudoLegalPawnMoves (Just enPassant) c = standardPawnMoves c ++ enPassantMoves enPassant where
  enPassantMoves  :: Coordinate -> [(Coordinate, Coordinate)]
  enPassantMoves  = undefined

standardPawnMoves :: Coordinate -> [(Coordinate, Coordinate)]
standardPawnMoves = undefined

pseudoLegalKnightMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalKnightMoves   = undefined

pseudoLegalBishopMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalBishopMoves   = undefined

pseudoLegalRookMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalRookMoves   = undefined

pseudoLegalQueenMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalQueenMoves   = undefined

pseudoLegalKingMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalKingMoves   = undefined

----------------------------------------------------------------------
-- PARSING
----------------------------------------------------------------------

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
