{-# LANGUAGE FlexibleInstances #-}

module Chess.Bitboard
  ( Bitboard(..)
  , BitboardRepresentation(..)
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
  , whiteKnightOccupancyFor
  , blackKnightOccupancyFor
  , whiteBishopOccupancyFor
  , blackBishopOccupancyFor
  , whiteRookOccupancyFor
  , blackRookOccupancyFor
  , whiteQueenOccupancyFor
  , blackQueenOccupancyFor
  , whiteKingOccupancyFor
  , blackKingOccupancyFor

  , regularToBitboard

  , translateNorth

  , rankMask
  , fileMask
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
  isOccupied (Bitboard word) (rankIndex, fileIndex) = testBit word $ (63 - 8 * rankIndex + fileIndex)

data Bitboard = Bitboard Word64 deriving (Eq)

data BitboardRepresentation = BitboardRepresentation
  { whitePawns   :: Bitboard
  , blackPawns   :: Bitboard
  , whiteBishops :: Bitboard
  , blackBishops :: Bitboard
  , whiteKnights :: Bitboard
  , blackKnights :: Bitboard
  , whiteRooks   :: Bitboard
  , blackRooks   :: Bitboard
  , whiteQueens  :: Bitboard
  , blackQueens  :: Bitboard
  , whiteKings   :: Bitboard
  , blackKings   :: Bitboard
  } deriving (Eq, Show)

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
  show (Bitboard b) = "\n" ++ (intercalate "\n" $
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

occupancyFor   :: Piece -> RegularBoardRepresentation -> Bitboard
occupancyFor p = snd . foldr (addPieceToBitboard p) (0, Bitboard 0) . concat where

  addPieceToBitboard            :: Piece -> Square -> (Int, Bitboard) -> (Int, Bitboard)
  addPieceToBitboard p s (i, b) | isRelevantPiece p s = (i + 1, Bitboard (2^(63 - i)) `bitboardUnion` b)
                                | otherwise         = (i + 1, b)

  isRelevantPiece     :: Piece -> Square -> Bool
  isRelevantPiece p s = pieceOn s == Just p

whitePawnOccupancyFor :: RegularBoardRepresentation -> Bitboard
whitePawnOccupancyFor = occupancyFor (Piece Pawn White)

blackPawnOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackPawnOccupancyFor = occupancyFor (Piece Pawn Black)

whiteKnightOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteKnightOccupancyFor = occupancyFor (Piece Knight White)

blackKnightOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackKnightOccupancyFor = occupancyFor (Piece Knight Black)

whiteBishopOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteBishopOccupancyFor = occupancyFor (Piece Bishop White)

blackBishopOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackBishopOccupancyFor = occupancyFor (Piece Bishop Black)

whiteRookOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteRookOccupancyFor = occupancyFor (Piece Rook White)

blackRookOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackRookOccupancyFor = occupancyFor (Piece Rook Black)

whiteQueenOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteQueenOccupancyFor = occupancyFor (Piece Queen White)

blackQueenOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackQueenOccupancyFor = occupancyFor (Piece Queen Black)

whiteKingOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteKingOccupancyFor = occupancyFor (Piece King White)

blackKingOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackKingOccupancyFor = occupancyFor (Piece King Black)

regularToBitboard   :: RegularBoardRepresentation -> BitboardRepresentation
regularToBitboard b = BitboardRepresentation
  { whitePawns   = whitePawnOccupancyFor b
  , blackPawns   = blackPawnOccupancyFor b
  , whiteBishops = whiteBishopOccupancyFor b
  , blackBishops = blackBishopOccupancyFor b
  , whiteKnights = whiteKnightOccupancyFor b
  , blackKnights = blackKnightOccupancyFor b
  , whiteRooks   = whiteRookOccupancyFor b
  , blackRooks   = blackRookOccupancyFor b
  , whiteQueens  = whiteQueenOccupancyFor b
  , blackQueens  = blackQueenOccupancyFor b
  , whiteKings   = whiteKingOccupancyFor b
  , blackKings   = blackKingOccupancyFor b
  }

translateNorth :: Bitboard -> Bitboard
translateNorth (Bitboard b) = Bitboard $ rotateL b 8

rankMask :: Rank -> Bitboard
rankMask r = Bitboard $ shiftL 255 ((r * 8) .&. 56)

fileMask :: File -> Bitboard
fileMask f = Bitboard $ shiftL 72340172838076673 (fileIndex .&. 7) where
  fileIndex = fromEnum f - 97
