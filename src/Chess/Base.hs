module Chess.Base
  ( CastleRights(..),
    Coordinate(..),
    File,
    isOnBoard,
    Move(..),
    MoveType(..),
    Piece(..),
    PieceType(..),
    Player(..),
    Rank,
    RegularBoardRepresentation,
    RegularGame(..),
    Square(..),

    addPiece,
    coordinateEuclideanDistance,
    makeMove,
    offsetBy,
    rayFromMove,
    scaleBy,
    squareAt,
    unoccupied,
    unoccupiedByAlly
  ) where

import Control.Applicative
import Control.Monad.State.Lazy

import Data.Maybe

data Square = Square
  { pieceOn  :: Maybe Piece
  , location :: Coordinate
  } deriving(Eq, Read, Show)

data Piece = Piece
  { pieceType  :: PieceType
  , pieceOwner :: Player
  } deriving(Eq, Read)

data Move = Move
  { moveFrom     :: Coordinate
  , moveTo       :: Coordinate
  , moveType     :: MoveType
  } deriving(Eq, Read, Show)

data MoveType = Standard | Capture | Castle | Promotion | EnPassant deriving(Eq, Read, Show)

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

data Coordinate = Coordinate File Rank deriving(Eq, Read, Show)
type Rank   = Int
type File   = Char

-- KkQq
data CastleRights = CastleRights Bool Bool Bool Bool deriving(Eq, Show)

type RegularBoardRepresentation   = [[Square]]

data RegularGame = RegularGame
  { placement       :: RegularBoardRepresentation
  , activeColor     :: Player
  , castlingRights  :: CastleRights
  , enPassantSquare :: Maybe Coordinate
  , halfMoveClock   :: Int
  , fullMoveNumber  :: Int
  } deriving(Eq)

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
           ++ "  abcdefgh \n "
           ++ "\n"
           ++ (show $ activeColor g) ++ " to move\n"
           ++ (show $ castlingRights g) ++ "\n"
           ++ "En passant on " ++ (show $ enPassantSquare g) ++ "\n"
           ++ "Halfmove clock at " ++ (show $ halfMoveClock g) ++ "\n"
           ++ "Fullmove number " ++ (show $ fullMoveNumber g) ++ "\n" where
    eighthRank  = map pieceOn $ placement g !! 7
    seventhRank = map pieceOn $ placement g !! 6
    sixthRank   = map pieceOn $ placement g !! 5
    fifthRank   = map pieceOn $ placement g !! 4
    fourthRank  = map pieceOn $ placement g !! 3
    thirdRank   = map pieceOn $ placement g !! 2
    secondRank  = map pieceOn $ placement g !! 1
    firstRank   = map pieceOn $ head $ placement g

isOnBoard                  :: Coordinate -> Bool
isOnBoard (Coordinate f r) | f < 'a'   = False
                           | f > 'h'   = False
                           | r < 1     = False
                           | r > 8     = False
                           | otherwise = True

unoccupiedByAlly         :: RegularBoardRepresentation -> Coordinate -> Maybe Player -> Bool
unoccupiedByAlly b c ply | isNothing $ targetOwner = True
                         | ply /= targetOwner = True
                         | ply == targetOwner = False where
  targetPiece = pieceOn $ squareAt b c
  targetOwner = fmap pieceOwner $ targetPiece

unoccupied     :: RegularBoardRepresentation -> Coordinate -> Bool
unoccupied b c = isNothing . pieceOn $ squareAt b c

squareAt                    :: RegularBoardRepresentation -> Coordinate -> Square
squareAt b (Coordinate f r) = (b !! (r-1)) !! (fromEnum f - fromEnum 'a')

scaleBy                           :: Int -> (Int, Int) -> (Int, Int)
scaleBy s (x,y)                   = (x*s, y*s)

offsetBy                          :: Coordinate -> (Int, Int) -> Coordinate
offsetBy (Coordinate f r) (df,dr) = Coordinate (toEnum $ fromEnum f + df) (r + dr)

opponent               :: Player -> Player
opponent White         = Black
opponent Black         = White

coordinateEuclideanDistance                                       :: Coordinate -> Coordinate -> Int
coordinateEuclideanDistance (Coordinate cx y) (Coordinate cx' y') = ((x' - x) ^ 2) + ((y' - y) ^ 2) where
  x' = fromEnum cx' - fromEnum 'a'
  x  = fromEnum cx - fromEnum 'a'

rayFromMove                                        :: (Coordinate, Coordinate) -> (Int, Int)
rayFromMove ((Coordinate f r), (Coordinate f' r')) | fromEnum f' > fromEnum f && r' > r = (1,1)
                                                   | fromEnum f' > fromEnum f && r' < r = (1,-1)
                                                   | fromEnum f' > fromEnum f && r' == r = (1,0)
                                                   | fromEnum f' < fromEnum f && r' > r = (-1,1)
                                                   | fromEnum f' < fromEnum f && r' < r = (-1,-1)
                                                   | fromEnum f' < fromEnum f && r' == r = (-1,0)
                                                   | fromEnum f' == fromEnum f && r' > r = (0,1)
                                                   | fromEnum f' == fromEnum f && r' < r = (0,-1)
                                                   | fromEnum f' == fromEnum f && r' == r = (0,0)

pairEuclideanDistance               :: (Int, Int) -> (Int, Int) -> Int
pairEuclideanDistance (x,y) (x',y') = ((x' - x) ^ 2) + ((y' - y) ^ 2)

addPiece                        :: RegularBoardRepresentation -> Maybe Piece -> Coordinate -> RegularBoardRepresentation
addPiece b p c@(Coordinate f r) = newPlacement where
  newPlacement = fst splitBoard ++ [fst splitRank ++ [Square p c] ++ (tail . snd $ splitRank)] ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) $ b
  splitRank = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard

updateSquare     :: Coordinate -> Maybe Piece -> State RegularGame ()
updateSquare c p = do
  game <- get
  let position = placement game
  put $ game { placement = addPiece position p c }

makeStandardMove :: Move -> State RegularGame Bool
makeStandardMove Move { moveFrom = from
                      , moveTo   = to
                      , moveType = movetype } = do
  game <- get
  let position = placement game
  let originalPiece = pieceOn $ squareAt position from
  put $ game { activeColor = opponent (activeColor game) }

  updateSquare from Nothing
  updateSquare to originalPiece
  return True

disableWhiteCastles :: CastleRights -> CastleRights
disableWhiteCastles (CastleRights woo boo wooo booo) = CastleRights False boo False booo

disableBlackCastles :: CastleRights -> CastleRights
disableBlackCastles (CastleRights woo boo wooo booo) = CastleRights woo False wooo False

doCastle :: (CastleRights -> CastleRights) -> Player -> Move -> Move -> State RegularGame Bool
doCastle fupdaterights ply Move { moveFrom = from, moveTo = to } Move { moveFrom = rookfrom
                                                                      , moveTo   = rookto } = do
  game <- get
  let position = placement game
  let originalPiece = pieceOn $ squareAt position from
  put $ game { activeColor = opponent (activeColor game)
             , castlingRights = fupdaterights (castlingRights game)
             }

  updateSquare from Nothing
  updateSquare to originalPiece
  updateSquare rookfrom Nothing
  updateSquare rookto (Just $ Piece Rook ply)

  return True

makeWhiteKingsideCastle      :: Move -> State RegularGame Bool
makeWhiteKingsideCastle move = doCastle disableWhiteCastles White move
  (Move { moveFrom = (Coordinate 'h' 1)
        , moveTo   = (Coordinate 'f' 1)
        , moveType = Castle
        })

makeWhiteQueensideCastle      :: Move -> State RegularGame Bool
makeWhiteQueensideCastle move = doCastle disableWhiteCastles White move
  (Move { moveFrom = (Coordinate 'a' 1)
        , moveTo   = (Coordinate 'd' 1)
        , moveType = Castle
        })

makeBlackKingsideCastle      :: Move -> State RegularGame Bool
makeBlackKingsideCastle move = doCastle disableBlackCastles Black move
  (Move { moveFrom = (Coordinate 'h' 8)
        , moveTo   = (Coordinate 'f' 8)
        , moveType = Castle
        })

makeBlackQueensideCastle      :: Move -> State RegularGame Bool
makeBlackQueensideCastle move = doCastle disableBlackCastles Black move
  (Move { moveFrom = (Coordinate 'a' 8)
        , moveTo   = (Coordinate 'd' 8)
        , moveType = Castle
        })

makeCastle                              :: Move -> State RegularGame Bool
makeCastle move@Move { moveFrom = from
                     , moveTo   = to }  | from == Coordinate 'e' 1 && to == Coordinate 'g' 1 = makeWhiteKingsideCastle move
                                        | from == Coordinate 'e' 1 && to == Coordinate 'c' 1 = makeWhiteQueensideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'g' 8 = makeBlackKingsideCastle move
                                        | from == Coordinate 'e' 8 && to == Coordinate 'c' 8 = makeBlackQueensideCastle move

makePromotion :: Maybe Piece -> Move -> State RegularGame Bool
makePromotion p@(Just Piece { pieceType  = pt, pieceOwner = o }) move@Move { moveFrom = from, moveTo = to } = do
  game <- get
  let position = placement game
  put $ game { activeColor = opponent (activeColor game) }

  updateSquare from Nothing
  updateSquare to p
  return True

makeEnPassant   :: Move -> State RegularGame Bool
makeEnPassant m@Move { moveTo = to@(Coordinate f r)
                     , moveFrom = from } = do
  game <- get
  let position = placement game
  let originalPiece = pieceOn $ squareAt position from
  let rankOffset = if fmap pieceOwner originalPiece == (Just White) then (-1) else 1
  put $ game { activeColor = opponent (activeColor game) }

  let enpassantpawn = pieceOn . squareAt position $ Coordinate f (r+rankOffset)

  updateSquare from Nothing
  updateSquare (Coordinate f (r+rankOffset)) Nothing
  updateSquare to originalPiece
  return True


makeMove                                             :: Maybe Piece -> Move -> State RegularGame Bool
makeMove promoteTo move@Move { moveType = movetype } | movetype == Standard  = makeStandardMove move
                                                     | movetype == Capture   = makeStandardMove move
                                                     | movetype == Castle    = makeCastle move
                                                     | movetype == Promotion = makePromotion promoteTo move
                                                     | movetype == EnPassant = makeEnPassant move
