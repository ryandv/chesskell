{-# LANGUAGE FlexibleInstances #-}

module Chess.Bitboard
  ( Bitboard(..)
  , bitboardIntersect
  , bitboardUnion
  , BoardIndex
  , emptyBitboard
  , isOccupied
  , turnWord64IntoWord8s
  , extractWord8
  , printWord8AsBinary
  , whitePawnOccupancyFor
  , blackPawnOccupancyFor
  ) where

import Chess.Base

import Data.Bits
import Data.Functor
import Data.List
import Data.Word
import Text.Show

class (Show a) => BoardIndex a where
  isOccupied :: Bitboard -> a -> Bool

instance BoardIndex Int where
  isOccupied (Bitboard word) squareIndex = testBit word squareIndex

instance BoardIndex (Int, Int) where
  isOccupied (Bitboard word) (rankIndex, fileIndex) = testBit word $ 8 * rankIndex + fileIndex

data Bitboard = Bitboard Word64 deriving (Eq)

emptyBitboard :: Bitboard
emptyBitboard = Bitboard 0

bitboardIntersect                             :: Bitboard -> Bitboard -> Bitboard
bitboardIntersect (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..&. b2

bitboardUnion                             :: Bitboard -> Bitboard -> Bitboard
bitboardUnion (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..|. b2

extractWord8   :: Word64 -> Word8
extractWord8 w = fromIntegral $ w Data.Bits..&. mask where
  mask :: Word64
  mask = fromInteger $ toInteger 255

turnWord64IntoWord8s :: Word64 -> [Word8]
turnWord64IntoWord8s w = map (extractWord8 . (shiftR w)) [56, 48, 40, 32, 24, 16, 8, 0]

instance Show Bitboard where
  show (Bitboard b) = (intercalate "\n" $
    map printWord8AsBinary $
    turnWord64IntoWord8s b) ++ "\n"

printWord8AsBinary   :: Word8 -> String
printWord8AsBinary w = intersperse ' ' $ map bitToSquare wordAsBitString where

  wordAsBitString :: String
  wordAsBitString = foldr (flip (++) . getNthBit) "" (reverse [0..7])

  getNthBit :: Int -> String
  getNthBit bitIndex = show (((nthBitAtFront w bitIndex) Data.Bits..&. (fromIntegral 1 :: Word8)))

  bitToSquare :: Char -> Char
  bitToSquare '0' = '.'
  bitToSquare _   = '1'

  nthBitAtFront :: Word8 -> Int -> Word8
  nthBitAtFront w bitIndex = shiftR w bitIndex


whitePawnOccupancyFor :: RegularBoardRepresentation -> Bitboard
whitePawnOccupancyFor = pawnOccupancyFor White

blackPawnOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackPawnOccupancyFor = pawnOccupancyFor Black

pawnOccupancyFor   :: Player -> RegularBoardRepresentation -> Bitboard
pawnOccupancyFor p = snd . foldr addPieceToBitboard (0, Bitboard 0) . concat where

  addPieceToBitboard          :: Square -> (Int, Bitboard) -> (Int, Bitboard)
  addPieceToBitboard s (i, b) | isPawnForPlayer p s = (i + 1, Bitboard (2^i) `bitboardUnion` b)
                              | otherwise         = (i + 1, b)

  isPawnForPlayer     :: Player -> Square -> Bool
  isPawnForPlayer p s = pieceOn s == Just (Piece Pawn p)
