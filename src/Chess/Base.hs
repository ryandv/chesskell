module Chess.Base
  ( CastleRights(..),
    Coordinate(..),
    File,
    isOnBoard,
    Piece(..),
    PieceType(..),
    Player(..),
    Rank,
    RegularBoardRepresentation,
    RegularGame(..),
    Square(..),

    potentialKnightMoves,
    potentialPawnMoves,
    pseudoLegalMoves,
    squareAt
  ) where

import Control.Applicative

import Data.Maybe

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

data Coordinate = Coordinate File Rank deriving(Read, Show)
type Rank   = Integer
type File   = Char

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
isOnBoard (Coordinate f r) | f < 'a'   = False
                           | f > 'h'   = False
                           | r < 1     = False
                           | r > 8     = False
                           | otherwise = True

pseudoLegalMoves               :: RegularGame -> [(Coordinate, Coordinate)]
pseudoLegalMoves RegularGame { placement = b
                             , enPassantSquare = e
                             } = (concatMap . concatMap) (pseudoLegalMovesFrom e b) b where

pseudoLegalMovesFrom :: Maybe Coordinate -> RegularBoardRepresentation -> Square -> [(Coordinate, Coordinate)]
pseudoLegalMovesFrom _ _ (Square Nothing _)            = []
pseudoLegalMovesFrom c b (Square (Just (Piece p _)) l) | p == Pawn   = filter (unoccupied b . snd) $ potentialPawnMoves b c l
                                                       | p == Knight = filter (unoccupied b . snd) $ potentialKnightMoves l
                                                       | p == Bishop = filter (unoccupied b . snd) $ potentialBishopMoves l
                                                       | p == Rook   = filter (unoccupied b . snd) $ potentialRookMoves l
                                                       | p == Queen  = filter (unoccupied b . snd) $ potentialQueenMoves l
                                                       | p == King   = filter (unoccupied b . snd) $ potentialKingMoves l

unoccupied     :: RegularBoardRepresentation -> Coordinate -> Bool
unoccupied b c = isNothing . pieceOn $ squareAt b c

squareAt                    :: RegularBoardRepresentation -> Coordinate -> Square
squareAt b (Coordinate f r) = (b !! fromInteger (r-1)) !! (fromEnum f - fromEnum 'a')

scaleBy                           :: Integer -> (Integer, Integer) -> (Integer, Integer)
scaleBy s (x,y)                   = (x*s, y*s)

offsetBy                          :: Coordinate -> (Integer, Integer) -> Coordinate
offsetBy (Coordinate f r) (df,dr) = Coordinate (toEnum $ fromEnum f + fromInteger df) (r + dr)


potentialPawnMoves                                       :: RegularBoardRepresentation -> Maybe Coordinate -> Coordinate -> [(Coordinate, Coordinate)]
potentialPawnMoves b Nothing c@(Coordinate r f)          = standardPawnMoves b c
potentialPawnMoves b (Just enPassant) c@(Coordinate r f) = standardPawnMoves b c ++ enPassantMoves enPassant where
  enPassantMoves                      :: Coordinate -> [(Coordinate, Coordinate)]
  enPassantMoves d@(Coordinate r' f') | r' == r && (f == (f' - 1) || f == (f' + 1)) = [((Coordinate r f), (Coordinate r' f'))]
                                      | otherwise                                   = []

standardPawnMoves                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
standardPawnMoves b c@(Coordinate f r) | r == 2    = doubleJump b c ++ advance b c ++ captures b c
                                       | r == 7    = doubleJump b c ++ advance b c ++ captures b c
                                       | otherwise = advance b c ++ captures b c

advance                            :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
advance b c@(Coordinate f r)       = maybe [] (\p -> case (pieceOwner p) of
                                                       White -> [((Coordinate f r), (Coordinate f (r+1)))]
                                                       Black -> [((Coordinate f r), (Coordinate f (r-1)))])
                                              (pieceOn (squareAt b c))

doubleJump                         :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
doubleJump b c@(Coordinate f r)    = maybe [] (\p -> case (pieceOwner p) of
                                                       White -> [((Coordinate f r), (Coordinate f (r+2)))]
                                                       Black -> [((Coordinate f r), (Coordinate f (r-2)))])
                                              (pieceOn (squareAt b c))

captures                           :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
captures b c@(Coordinate f r)      = nwCapture b c ++ neCapture b c where

  nwCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
  nwCapture b c@(Coordinate f r) = maybe [] (\p -> case (pieceOwner p) of
                                                     White -> if (isJust $ whiteTarget) && fmap pieceOwner whiteTarget == Just Black
                                                                then [((Coordinate f r), (Coordinate (pred f) (r+1)))]
                                                                else []
                                                     Black -> if (isJust $ blackTarget) && fmap pieceOwner blackTarget == Just White
                                                              then [((Coordinate f r), (Coordinate (pred f) (r-1)))]
                                                              else [])
                                              (pieceOn (squareAt b c)) where
    whiteTarget = pieceOn $ squareAt b (Coordinate (pred f) (r+1))
    blackTarget = pieceOn $ squareAt b (Coordinate (pred f) (r-1))


  neCapture                      :: RegularBoardRepresentation -> Coordinate -> [(Coordinate, Coordinate)]
  neCapture b c@(Coordinate f r) = maybe [] (\p -> case (pieceOwner p) of
                                                     White -> if (isJust $ whiteTarget) && fmap pieceOwner whiteTarget == Just Black
                                                                then [((Coordinate f r), (Coordinate (succ f) (r+1)))]
                                                                else []
                                                     Black -> if (isJust $ blackTarget) && fmap pieceOwner blackTarget == Just White
                                                              then [((Coordinate f r), (Coordinate (succ f) (r-1)))]
                                                              else [])
                                              (pieceOn (squareAt b c)) where
    whiteTarget = pieceOn $ squareAt b (Coordinate (succ f) (r+1))
    blackTarget = pieceOn $ squareAt b (Coordinate (succ f) (r-1))
--captures b c@(Coordinate f r)      | isNothing . pieceOn $ squareAt b (Coordinate (succ f) r) && isNothing . pieceOn $ squareAt b (Coordinate (succ f) r)= []
--                                   | isJust . pieceOn $ squareAt b (Coordinate (su
--captures b c@(Coordinate f r)      = do
--  let target = maybeToList $ (pieceOn (squareAt b (Coordinate (succ f) r)))
--  let origin = maybeToList $ (pieceOn (squareAt b c))
--  case target of
--    [] -> []
--    [Piece _ (opponent . pieceOwner $ origin)] -> []

--captures b c@(Coordinate f r)      = maybe [] (\p -> case (pieceOwner p) of
--                                                       White -> [((Coordinate r f), (Coordinate f (r+1)))]
--                                                       Black -> [((Coordinate r f), (Coordinate f (r-1)))])
--                                              (pieceOn (squareAt b c))

opponent               :: Player -> Player
opponent White         = Black
opponent Black         = White

potentialKnightMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialKnightMoves c = fmap (\x -> (c,x)) $ filter isOnBoard $ fmap (c `offsetBy`) possibleJumps where
  possibleJumps = [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1)]

potentialRayMoves           :: Coordinate -> [(Integer, Integer)] -> [(Coordinate, Coordinate)]
potentialRayMoves c offsets = fmap (\x -> (c,x)) $ filter isOnBoard $ fmap (c `offsetBy`) $ scaleBy <$> [1..7] <*> offsets

potentialBishopMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialBishopMoves c = potentialRayMoves c diagonals where
  diagonals = [(-1,1),(1,1),(1,-1),(-1,-1)]

potentialRookMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialRookMoves c = potentialRayMoves c straights where
  straights = [(1,0),(-1,0),(0,1),(0,-1)]

potentialQueenMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialQueenMoves c = potentialRookMoves c ++ potentialBishopMoves c

potentialKingMoves   :: Coordinate -> [(Coordinate, Coordinate)]
potentialKingMoves   = undefined
