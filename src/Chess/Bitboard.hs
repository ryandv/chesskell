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
  ) where

import Data.Bits
import Data.List
import Data.Word
import Text.Show

class (Show a) => BoardIndex a where
  isOccupied :: Bitboard -> a -> Bool

instance BoardIndex Int where
  isOccupied (Bitboard word) squareIndex = testBit word squareIndex

instance BoardIndex (Int, Int) where
  isOccupied (Bitboard word) (rankIndex, fileIndex) = testBit word $ 8 * rankIndex + fileIndex

instance Show Bitboard where
  show (Bitboard b) = (intercalate "\n" $
    map printWord8AsBinary $
    turnWord64IntoWord8s b) ++ "\n"

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

printWord8AsBinary   :: Word8 -> String
printWord8AsBinary w = intersperse ' ' $ map bitToSquare wordAsBitString where

  wordAsBitString = foldr (\bitIndex acc -> acc ++ (show (((nthBitAtFront w bitIndex) Data.Bits..&. (fromIntegral 1 :: Word8))))) "" (reverse [0..7])

  bitToSquare :: Char -> Char
  bitToSquare '0' = '.'
  bitToSquare _   = '1'

  nthBitAtFront :: Word8 -> Int -> Word8
  nthBitAtFront w bitIndex = shiftR w bitIndex
