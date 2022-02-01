{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess.Base
  ( CastleRights(..)
  , CastleSide(..)
  , Coordinate(..)
  , File
  , Game(..)
  , isOnBoard
  , Move(..)
  , moveFrom
  , moveTo
  , Piece(..)
  , PieceType(..)
  , Player(..)
  , Rank
  , RegularBoardRepresentation
  , RegularGame(..)
  , Square(..)
  , opponent
  ) where

import           Control.Applicative
import           Control.Monad.State.Lazy

import           Data.List
import           Data.Maybe

data Square = Square
  { pieceOn  :: Maybe Piece
  , location :: Coordinate
  }
  deriving (Eq, Read, Show)

data Piece = Piece
  { pieceType  :: PieceType
  , pieceOwner :: Player
  }
  deriving (Eq, Read)

data Move = Move Coordinate Coordinate | Capture Coordinate Coordinate | Castle Coordinate Coordinate | EnPassant Coordinate Coordinate | Promote Coordinate Coordinate Piece
  deriving (Eq, Read, Show)

moveFrom :: Move -> Coordinate
moveFrom (Move f _) = f
moveFrom (Capture f _) = f
moveFrom (Castle f _) = f
moveFrom (EnPassant f _) = f
moveFrom (Promote f _ _) = f

moveTo :: Move -> Coordinate
moveTo (Move _ t) = t
moveTo (Capture _ t) = t
moveTo (Castle _ t) = t
moveTo (EnPassant _ t) = t
moveTo (Promote _ t _) = t

instance Show Piece where
  show (Piece Rook   White) = "R"
  show (Piece Knight White) = "N"
  show (Piece Bishop White) = "B"
  show (Piece Queen  White) = "Q"
  show (Piece King   White) = "K"
  show (Piece Pawn   White) = "P"

  show (Piece Rook   Black) = "r"
  show (Piece Knight Black) = "n"
  show (Piece Bishop Black) = "b"
  show (Piece Queen  Black) = "q"
  show (Piece King   Black) = "k"
  show (Piece Pawn   Black) = "p"

data PieceType  = Pawn | Knight | Bishop | Rook | Queen | King deriving (Enum, Eq, Read, Show)
data Player = White | Black deriving (Enum, Eq, Read, Show)

data Coordinate = Coordinate File Rank
  deriving (Eq, Read, Show)
type Rank = Int
type File = Char

-- KkQq
data CastleRights = CastleRights Bool Bool Bool Bool
  deriving (Eq, Show)
data CastleSide = Queenside | Kingside deriving(Eq, Show)

type RegularBoardRepresentation = [[Square]]

data Game r = Game
  { placement       :: r
  , activeColor     :: Player
  , castlingRights  :: CastleRights
  , enPassantSquare :: Maybe Coordinate
  , halfMoveClock   :: Int
  , fullMoveNumber  :: Int
  } deriving(Eq)

type RegularGame = Game RegularBoardRepresentation

instance Show (Game RegularBoardRepresentation) where
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
           ++ show (activeColor g) ++ " to move\n"
           ++ show ( castlingRights g) ++ "\n"
           ++ "En passant on " ++ show (enPassantSquare g) ++ "\n"
           ++ "Halfmove clock at " ++ show (halfMoveClock g) ++ "\n"
           ++ "Fullmove number " ++ show (fullMoveNumber g) ++ "\n" where
    eighthRank  = map pieceOn $ placement g !! 7
    seventhRank = map pieceOn $ placement g !! 6
    sixthRank   = map pieceOn $ placement g !! 5
    fifthRank   = map pieceOn $ placement g !! 4
    fourthRank  = map pieceOn $ placement g !! 3
    thirdRank   = map pieceOn $ placement g !! 2
    secondRank  = map pieceOn $ placement g !! 1
    firstRank   = map pieceOn $ head $ placement g

isOnBoard :: Coordinate -> Bool
isOnBoard (Coordinate f r) | f < 'a'   = False
                           | f > 'h'   = False
                           | r < 1     = False
                           | r > 8     = False
                           | otherwise = True

opponent :: Player -> Player
opponent White = Black
opponent Black = White
