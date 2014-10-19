module Chess.Base
  ( CastleRights(..),
    Coordinate(..),
    File,
    Piece(..),
    PieceType(..),
    Player(..),
    Rank,
    RegularBoardRepresentation,
    RegularGame(..),
    Square(..),

    pseudoLegalMoves
  ) where

import Control.Applicative

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
    firstRank   = map pieceOn $ head $ placement g

isOnBoard                  :: Coordinate -> Bool
isOnBoard (Coordinate r f) | r < 'a'   = False
                           | r > 'h'   = False
                           | f < 1     = False
                           | f > 8     = False
                           | otherwise = True

pseudoLegalMoves               :: RegularGame -> [(Coordinate, Coordinate)]
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

scaleBy                           :: Integer -> (Integer, Integer) -> (Integer, Integer)
scaleBy s (x,y)                   = (x*s, y*s)

offsetBy                          :: Coordinate -> (Integer, Integer) -> Coordinate
offsetBy (Coordinate r f) (dr,df) = Coordinate (toEnum . fromInteger . (+ dr) . toInteger . fromEnum $ r) (f + df)


pseudoLegalPawnMoves                          :: Maybe Coordinate -> Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalPawnMoves Nothing c                = standardPawnMoves c
pseudoLegalPawnMoves (Just enPassant) c = standardPawnMoves c ++ enPassantMoves enPassant where
  enPassantMoves  :: Coordinate -> [(Coordinate, Coordinate)]
  enPassantMoves  = undefined

standardPawnMoves :: Coordinate -> [(Coordinate, Coordinate)]
standardPawnMoves = undefined

-- not really pseudo legal - does not check occupancy.
pseudoLegalKnightMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalKnightMoves c = fmap (\x -> (c,x)) $ filter isOnBoard $ fmap (c `offsetBy`) possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]

pseudoLegalBishopMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalBishopMoves c = fmap (\x -> (c,x)) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

pseudoLegalRookMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalRookMoves   = undefined

pseudoLegalQueenMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalQueenMoves   = undefined

pseudoLegalKingMoves   :: Coordinate -> [(Coordinate, Coordinate)]
pseudoLegalKingMoves   = undefined
