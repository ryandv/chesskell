{-# LANGUAGE FlexibleInstances #-}

module Chess.Bitboard
  ( Bitboard(..)
  , BitboardRepresentation(..)
  , bitboardIntersect
  , bitboardUnion
  , BoardIndex
  , indicesToCoordinate
  , indicesToSquareIndex
  , squareIndexToIndices
  , coordinateToIndices
  , bitboardToCoordinates
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
  , diagonalMask
  , antiDiagonalMask

  , northRay
  , eastRay
  , southRay
  , westRay

  , northEastRay
  , southEastRay
  , southWestRay
  , northWestRay

  , bitscanForward
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
  isOccupied (Bitboard word) = testBit word . indicesToSquareIndex

instance BoardIndex Coordinate where
  isOccupied b c = isOccupied b $ coordinateToIndices c

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

indicesToCoordinate :: (Int, Int) -> Coordinate
indicesToCoordinate (r, f) = Coordinate (toEnum $ 97 + f) (r + 1)

indicesToSquareIndex :: (Int, Int) -> Int
indicesToSquareIndex (r, f) = 8 * r + f

squareIndexToIndices :: Int -> (Int, Int)
squareIndexToIndices i = (i `div` 8, i `mod` 8)

coordinateToIndices :: Coordinate -> (Int, Int)
coordinateToIndices (Coordinate f r) = (r - 1, fromEnum f - 97)

bitboardToCoordinates :: Bitboard -> [Coordinate]
bitboardToCoordinates (Bitboard bits) = map (indicesToCoordinate . squareIndexToIndices) $ foldr getIndicesOfSetBits [] [0..63]
  where
    getIndicesOfSetBits exponent acc | testBit bits exponent = exponent:acc
                                     | otherwise             = acc

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
    turnWord64IntoWord8s b) ++ "\n" ++
    (show b)

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

rankMask :: Int -> Bitboard
rankMask r = Bitboard $ shiftL 255 ((r * 8) .&. 56)

fileMask :: Int -> Bitboard
fileMask f = Bitboard $ shiftL 72340172838076673 (f .&. 7) where

diagonalMask :: Int -> Bitboard
diagonalMask diagonal = Bitboard $ shiftL (shiftR 9241421688590303745 south) north where
  diag = (-8) * diagonal
  north = ((-1) * diag) .&. (shiftR diag 31)
  south = diag .&. (shiftR ((-1) * diag) 31)

antiDiagonalMask :: Int -> Bitboard
antiDiagonalMask antiDiagonal = Bitboard $ shiftL (shiftR 72624976668147840 south) north where
  diag = 56 - 8 * antiDiagonal
  north = ((-1) * diag) .&. (shiftR diag 31)
  south = diag .&. (shiftR ((-1) * diag) 31)

northRay              :: (Int, Int) -> Bitboard
northRay (rank, file) = positiveRayFromLine (fileMask file) (rank, file)

eastRay :: (Int, Int) -> Bitboard
eastRay (rank, file) = positiveRayFromLine (rankMask rank) (rank, file)

southRay :: (Int, Int) -> Bitboard
southRay (rank, file) = negativeRayFromLine (fileMask file) (rank, file)

westRay :: (Int, Int) -> Bitboard
westRay (rank, file) = negativeRayFromLine (rankMask rank) (rank, file)

northEastRay :: (Int, Int) -> Bitboard
northEastRay (rank, file) = positiveRayFromLine (diagonalMask $ rank - file) (rank, file)

southEastRay :: (Int, Int) -> Bitboard
southEastRay (rank, file) = negativeRayFromLine (antiDiagonalMask $ rank + file) (rank, file)

southWestRay :: (Int, Int) -> Bitboard
southWestRay (rank, file) = negativeRayFromLine (diagonalMask $ rank - file) (rank, file)

northWestRay :: (Int, Int) -> Bitboard
northWestRay (rank, file) = positiveRayFromLine (antiDiagonalMask $ rank + file) (rank, file)

positiveRayFromLine :: Bitboard -> (Int, Int) -> Bitboard
positiveRayFromLine (Bitboard bits) (rank, file) = Bitboard $ bits .&. (shiftL (-2) (indicesToSquareIndex (rank, file)))

negativeRayFromLine :: Bitboard -> (Int, Int) -> Bitboard
negativeRayFromLine (Bitboard bits) (rank, file) = Bitboard $ bits .&. ((shiftL 1 (indicesToSquareIndex (rank, file))) - 1)

bitIndexTable :: [Int]
bitIndexTable =
  [ 64,  0,  1, 39,  2, 15, 40, 23
  ,  3, 12, 16, 59, 41, 19, 24, 54
  ,  4, -1, 13, 10, 17, 62, 60, 28
  , 42, 30, 20, 51, 25, 44, 55, 47
  ,  5, 32, -1, 38, 14, 22, 11, 58
  , 18, 53, 63,  9, 61, 27, 29, 50
  , 43, 46, 31, 37, 21, 57, 52,  8
  , 26, 49, 45, 36, 56,  7, 48, 35
  ,  6, 34, 33, -1 ]

bitscanForward :: Bitboard -> Int
bitscanForward (Bitboard bits) = bitIndexTable !! (fromEnum lookupIndex)
  where lookupIndex = bits .&. ((-1) * bits) `mod` 67
