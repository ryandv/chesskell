{-# LANGUAGE FlexibleInstances #-}

module Chess.Bitboard
  ( BoardIndex
  , emptyBitboard
  , isOccupied
  ) where

import Data.Bits
import Data.Word

class (Show a) => BoardIndex a where
  isOccupied :: Bitboard -> a -> Bool

instance BoardIndex Int where
  isOccupied (Bitboard word) squareIndex = testBit word squareIndex

instance BoardIndex (Int, Int) where
  isOccupied (Bitboard word) (rankIndex, fileIndex) = testBit word $ 8 * rankIndex + fileIndex

data Bitboard = Bitboard Word64

emptyBitboard :: Bitboard
emptyBitboard = Bitboard 0

