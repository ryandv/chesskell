module Chess.Bitboard
  ( emptyBitboard
  , isOccupied
  ) where

import Data.Bits
import Data.Word

data Bitboard = Bitboard Word64

emptyBitboard :: Bitboard
emptyBitboard = Bitboard 0

isOccupied                             :: Bitboard -> Int -> Bool
isOccupied (Bitboard word) squareIndex = testBit word squareIndex
