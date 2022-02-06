{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Chess.Bitboard
  ( addPieceTo
  , Bitboard(..)
  , BitboardRepresentation(..)
  , bitboardIntersect
  , bitboardUnion
  , bitboardComplement
  , bitboardXOR
  , bitboardPieceAt
  , bitboardOwnerAt
  , bitboardSpecificPieceAt
  , bitboardIsOccupied
  , BoardIndex
  , emptyBitboardRepresentation
  , indicesToCoordinate
  , indicesToSquareIndex
  , squareIndexToIndices
  , coordinateToIndices
  , bitboardToCoordinates
  , bitboardToSquareIndices
  , emptyBitboard
  , isOccupied
  , turnWord64IntoWord8s
  , extractWord8
  , printWord8AsBinary
  , regularGameToBitboardGame
  , bitboardMovePiece
  , whiteOccupancyFor
  , blackOccupancyFor
  , totalOccupancyFor
  , occupiedCoordinates
  , regularToBitboard
  , singleOccupant
  , translateNorth
  , rankMask
  , removePieceFrom
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
  , bitscanReverse
  ) where

import           Chess.Base

import           Data.Bits
import           Data.Functor
import           Data.List
import           Data.Maybe
import qualified Data.Strict.Tuple as T
import           Data.Word
import           Text.Show

class (Show a) => BoardIndex a where
  isOccupied :: Bitboard -> a -> Bool

instance BoardIndex Int where
  isOccupied (Bitboard word) squareIndex = testBit word squareIndex

instance BoardIndex (Int, Int) where
  isOccupied (Bitboard word) (!r, !f) = testBit word $ 8 * r + f

instance BoardIndex (Int T.:!: Int) where
  isOccupied (Bitboard word) (r T.:!: f) = testBit word $ 8 * r + f

instance BoardIndex Coordinate where
  isOccupied b c = isOccupied b $ coordinateToIndices c

data Bitboard = Bitboard Word64
  deriving Eq

data BitboardRepresentation = BitboardRepresentation
  { whitePawns     :: Bitboard
  , blackPawns     :: Bitboard
  , whiteBishops   :: Bitboard
  , blackBishops   :: Bitboard
  , whiteKnights   :: Bitboard
  , blackKnights   :: Bitboard
  , whiteRooks     :: Bitboard
  , blackRooks     :: Bitboard
  , whiteQueens    :: Bitboard
  , blackQueens    :: Bitboard
  , whiteKings     :: Bitboard
  , blackKings     :: Bitboard
  , totalOccupancy :: Bitboard
  , whiteOccupancy :: Bitboard
  , blackOccupancy :: Bitboard
  }
  deriving (Eq, Show)

instance Show (Game BitboardRepresentation) where
  show g = show (placement g) ++ "\n"
           ++ show (activeColor g) ++ " to move\n"
           ++ show ( castlingRights g) ++ "\n"
           ++ "En passant on " ++ show (enPassantSquare g) ++ "\n"
           ++ "Halfmove clock at " ++ show (halfMoveClock g) ++ "\n"
           ++ "Fullmove number " ++ show (fullMoveNumber g) ++ "\n" where

emptyBitboardRepresentation :: BitboardRepresentation
emptyBitboardRepresentation = BitboardRepresentation
  { whitePawns     = emptyBitboard
  , blackPawns     = emptyBitboard
  , whiteBishops   = emptyBitboard
  , blackBishops   = emptyBitboard
  , whiteKnights   = emptyBitboard
  , blackKnights   = emptyBitboard
  , whiteRooks     = emptyBitboard
  , blackRooks     = emptyBitboard
  , whiteQueens    = emptyBitboard
  , blackQueens    = emptyBitboard
  , whiteKings     = emptyBitboard
  , blackKings     = emptyBitboard
  , totalOccupancy = emptyBitboard
  , whiteOccupancy = emptyBitboard
  , blackOccupancy = emptyBitboard
  }

indicesToCoordinate :: (Int T.:!: Int) -> Coordinate
indicesToCoordinate (r T.:!: f) = Coordinate (toEnum $ 97 + f) (r + 1)

indicesToSquareIndex :: (Int T.:!: Int) -> Int
indicesToSquareIndex (r T.:!: f) = 8 * r + f

squareIndexToIndices :: Int -> (Int T.:!: Int)
squareIndexToIndices i = (i `div` 8 T.:!: i `mod` 8)

coordinateToIndices :: Coordinate -> (Int T.:!: Int)
coordinateToIndices (Coordinate !f !r) = (r - 1 T.:!: fromEnum f - 97)

bitboardToSquareIndices :: Bitboard -> [Int]
bitboardToSquareIndices (Bitboard bits) = filter (testBit bits) [0 .. 63]

bitboardToCoordinates :: Bitboard -> [Coordinate]
bitboardToCoordinates (Bitboard bits) =
  map (indicesToCoordinate . squareIndexToIndices)
    . filter (\i -> testBit bits i)
    $ [0 .. 63]

emptyBitboard :: Bitboard
emptyBitboard = Bitboard 0

bitboardComplement :: Bitboard -> Bitboard
bitboardComplement (Bitboard bits) = Bitboard $ complement bits

bitboardIntersect :: Bitboard -> Bitboard -> Bitboard
bitboardIntersect (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..&. b2

bitboardUnion :: Bitboard -> Bitboard -> Bitboard
bitboardUnion (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..|. b2

bitboardXOR :: Bitboard -> Bitboard -> Bitboard
bitboardXOR (Bitboard b1) (Bitboard b2) = Bitboard $ b1 `Data.Bits.xor` b2

extractWord8 :: Word64 -> Word8
extractWord8 w = fromIntegral $ w Data.Bits..&. mask where
  mask :: Word64
  mask = fromInteger $ toInteger 255

turnWord64IntoWord8s :: Word64 -> [Word8]
turnWord64IntoWord8s w =
  map (extractWord8 . (shiftR w)) [56, 48, 40, 32, 24, 16, 8, 0]

instance Show Bitboard where
  show (Bitboard b) =
    "\n"
      ++ (intercalate "\n" $ map printWord8AsBinary $ turnWord64IntoWord8s b)
      ++ "\n"
      ++ (show b)

printWord8AsBinary :: Word8 -> String
printWord8AsBinary w = intersperse ' ' $ map bitToSquare wordAsBitString where

  wordAsBitString :: String
  wordAsBitString = foldr (flip (++) . getNthBit) "" (reverse [0 .. 7])

  getNthBit :: Int -> String
  getNthBit bitIndex =
    show (((nthBitAtFront w bitIndex) Data.Bits..&. (fromIntegral 1 :: Word8)))

  bitToSquare :: Char -> Char
  bitToSquare '0' = '.'
  bitToSquare _   = '1'

  nthBitAtFront :: Word8 -> Int -> Word8
  nthBitAtFront w bitIndex = shiftR w bitIndex

occupancyFor :: Piece -> RegularBoardRepresentation -> Bitboard
occupancyFor p = snd . foldr (addPieceToBitboard p) (0, Bitboard 0) . concat where

  addPieceToBitboard :: Piece -> Square -> (Int, Bitboard) -> (Int, Bitboard)
  addPieceToBitboard p s (i, b)
    | isRelevantPiece p s = (i + 1, Bitboard (2 ^ (63 - i)) `bitboardUnion` b)
    | otherwise           = (i + 1, b)

  isRelevantPiece :: Piece -> Square -> Bool
  isRelevantPiece p s = pieceOn s == Just p

whiteOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteOccupancyFor b =
  whitePawns bitboard
    `bitboardUnion` whiteBishops bitboard
    `bitboardUnion` whiteKnights bitboard
    `bitboardUnion` whiteRooks bitboard
    `bitboardUnion` whiteQueens bitboard
    `bitboardUnion` whiteKings bitboard
  where bitboard = regularToBitboard b

blackOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackOccupancyFor b =
  blackPawns bitboard
    `bitboardUnion` blackBishops bitboard
    `bitboardUnion` blackKnights bitboard
    `bitboardUnion` blackRooks bitboard
    `bitboardUnion` blackQueens bitboard
    `bitboardUnion` blackKings bitboard
  where bitboard = regularToBitboard b

totalOccupancyFor :: RegularBoardRepresentation -> Bitboard
totalOccupancyFor =
  foldr occupiedSquareToBitboard emptyBitboard
    . filter (isJust . pieceOn)
    . concat
 where
  occupiedSquareToBitboard occupiedSquare acc =
    Bitboard
        (shiftL
          1
          (indicesToSquareIndex . coordinateToIndices $ location occupiedSquare)
        )
      `bitboardUnion` acc

regularToBitboard :: RegularBoardRepresentation -> BitboardRepresentation
regularToBitboard b = withoutTotalOccupancy
  { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
  , whiteOccupancy = whiteOccupancy
  , blackOccupancy = blackOccupancy
  }

 where
  whiteOccupancy = (whitePawns withoutTotalOccupancy
                      `bitboardUnion` whiteBishops withoutTotalOccupancy
                      `bitboardUnion` whiteKnights withoutTotalOccupancy
                      `bitboardUnion` whiteRooks withoutTotalOccupancy
                      `bitboardUnion` whiteQueens withoutTotalOccupancy
                      `bitboardUnion` whiteKings withoutTotalOccupancy)
  blackOccupancy = (blackPawns withoutTotalOccupancy
                      `bitboardUnion` blackBishops withoutTotalOccupancy
                      `bitboardUnion` blackKnights withoutTotalOccupancy
                      `bitboardUnion` blackRooks withoutTotalOccupancy
                      `bitboardUnion` blackQueens withoutTotalOccupancy
                      `bitboardUnion` blackKings withoutTotalOccupancy)
  addPieceToBitboard square bitboards
    | pieceOn square == Just (Piece Pawn White) = bitboards
      { whitePawns = (whitePawns bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Pawn Black) = bitboards
      { blackPawns = (blackPawns bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Knight White) = bitboards
      { whiteKnights = (whiteKnights bitboards)
                         `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Knight Black) = bitboards
      { blackKnights = (blackKnights bitboards)
                         `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Bishop White) = bitboards
      { whiteBishops = (whiteBishops bitboards)
                         `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Bishop Black) = bitboards
      { blackBishops = (blackBishops bitboards)
                         `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Rook White) = bitboards
      { whiteRooks = (whiteRooks bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Rook Black) = bitboards
      { blackRooks = (blackRooks bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Queen White) = bitboards
      { whiteQueens = (whiteQueens bitboards)
                        `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece Queen Black) = bitboards
      { blackQueens = (blackQueens bitboards)
                        `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece King White) = bitboards
      { whiteKings = (whiteKings bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | pieceOn square == Just (Piece King Black) = bitboards
      { blackKings = (blackKings bitboards)
                       `bitboardUnion` (Bitboard $ squareToBitboard square)
      }
    | otherwise = bitboards

  withoutTotalOccupancy = foldr addPieceToBitboard emptyBitboardRepresentation $ concat b
  squareToBitboard square =
    shiftL 1 (indicesToSquareIndex . coordinateToIndices $ location square)

regularGameToBitboardGame :: RegularGame -> Game BitboardRepresentation
regularGameToBitboardGame regularGame = Game
  { placement       = regularToBitboard $ placement regularGame
  , activeColor     = activeColor regularGame
  , castlingRights  = castlingRights regularGame
  , enPassantSquare = enPassantSquare regularGame
  , halfMoveClock   = halfMoveClock regularGame
  , fullMoveNumber  = fullMoveNumber regularGame
  }

singleOccupant :: Coordinate -> Bitboard
singleOccupant coord =
  Bitboard $ shiftL 1 (indicesToSquareIndex . coordinateToIndices $ coord)

singleVacancy :: Coordinate -> Bitboard
singleVacancy coord = bitboardComplement (singleOccupant coord)

addPieceTo
  :: BitboardRepresentation
  -> Maybe Piece
  -> Coordinate
  -> BitboardRepresentation
addPieceTo bitboards piece coord
  | piece == Just (Piece Pawn White) = bitboards
    { whitePawns = (whitePawns bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Pawn Black) = bitboards
    { blackPawns = (blackPawns bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Knight White) = bitboards
    { whiteKnights = (whiteKnights bitboards)
                       `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Knight Black) = bitboards
    { blackKnights = (blackKnights bitboards)
                       `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Bishop White) = bitboards
    { whiteBishops = (whiteBishops bitboards)
                       `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Bishop Black) = bitboards
    { blackBishops = (blackBishops bitboards)
                       `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Rook White) = bitboards
    { whiteRooks = (whiteRooks bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Rook Black) = bitboards
    { blackRooks = (blackRooks bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Queen White) = bitboards
    { whiteQueens = (whiteQueens bitboards)
                      `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece Queen Black) = bitboards
    { blackQueens = (blackQueens bitboards)
                      `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece King White) = bitboards
    { whiteKings = (whiteKings bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | piece == Just (Piece King Black) = bitboards
    { blackKings = (blackKings bitboards) `bitboardUnion` (singleOccupant coord)
    }
  | otherwise = bitboards

removePieceFrom
  :: BitboardRepresentation
  -> Maybe Piece
  -> Coordinate
  -> BitboardRepresentation
removePieceFrom bitboards piece coord
  | piece == Just (Piece Pawn White) = bitboards
    { whitePawns = (whitePawns bitboards)
                     `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Pawn Black) = bitboards
    { blackPawns = (blackPawns bitboards)
                     `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Knight White) = bitboards
    { whiteKnights = (whiteKnights bitboards)
                       `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Knight Black) = bitboards
    { blackKnights = (blackKnights bitboards)
                       `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Bishop White) = bitboards
    { whiteBishops = (whiteBishops bitboards)
                       `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Bishop Black) = bitboards
    { blackBishops = (blackBishops bitboards)
                       `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Rook White) = bitboards
    { whiteRooks = (whiteRooks bitboards)
                     `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Rook Black) = bitboards
    { blackRooks = blackRooks bitboards
                     `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Queen White) = bitboards
    { whiteQueens = whiteQueens bitboards
                      `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece Queen Black) = bitboards
    { blackQueens = blackQueens bitboards
                      `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece King White) = bitboards
    { whiteKings = whiteKings bitboards
                     `bitboardIntersect` (singleVacancy coord)
    }
  | piece == Just (Piece King Black) = bitboards
    { blackKings = blackKings bitboards
                     `bitboardIntersect` (singleVacancy coord)
    }
  | otherwise = bitboards

bitboardMovePiece :: BitboardRepresentation -> Move -> BitboardRepresentation
bitboardMovePiece bitboards (Capture from to) =
  bitboardMovePieceCapture bitboards from to
bitboardMovePiece bitboards (Move from to) =
  bitboardMovePieceStandard bitboards from to
bitboardMovePiece bitboards (Castle from to) =
  bitboardMovePieceStandard bitboards from to
bitboardMovePiece bitboards (Promote from to p) =
  bitboardMovePiecePromote bitboards from to p
bitboardMovePiece bitboards (EnPassant from to) =
  bitboardMovePieceEnPassant bitboards from to

bitboardMovePieceEnPassant :: BitboardRepresentation -> Coordinate -> Coordinate -> BitboardRepresentation
bitboardMovePieceEnPassant bitboards from to@(Coordinate f r) =
  updatedBitboards
    { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
    , whiteOccupancy = whiteOccupancy
    , blackOccupancy = blackOccupancy
    }
 where
  whiteOccupancy = whitePawns updatedBitboards
                       `bitboardUnion` whiteBishops updatedBitboards
                       `bitboardUnion` whiteKnights updatedBitboards
                       `bitboardUnion` whiteRooks updatedBitboards
                       `bitboardUnion` whiteQueens updatedBitboards
                       `bitboardUnion` whiteKings updatedBitboards
  blackOccupancy = blackPawns updatedBitboards
                       `bitboardUnion` blackBishops updatedBitboards
                       `bitboardUnion` blackKnights updatedBitboards
                       `bitboardUnion` blackRooks updatedBitboards
                       `bitboardUnion` blackQueens updatedBitboards
                       `bitboardUnion` blackKings updatedBitboards
  updatedBitboards = addPieceTo
    (removePieceFrom (removePieceFrom bitboards capturedPawn capturedCoordinate)
                     capturingPawn
                     from
    )
    capturingPawn
    to

  capturingPawn = bitboardSpecificPieceAt bitboards from Pawn
  capturedPawn = bitboardSpecificPieceAt bitboards capturedCoordinate Pawn
  capturedCoordinate = Coordinate f (r + rankOffset)
  rankOffset = if (pieceOwner <$> capturingPawn) == Just White then (-1) else 1

bitboardMovePiecePromote :: BitboardRepresentation -> Coordinate -> Coordinate -> Piece -> BitboardRepresentation
bitboardMovePiecePromote bitboards from to p =
  updatedBitboards
    { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
    , whiteOccupancy = whiteOccupancy
    , blackOccupancy = blackOccupancy
    }
 where
  whiteOccupancy = whitePawns updatedBitboards
                   `bitboardUnion` whiteBishops updatedBitboards
                   `bitboardUnion` whiteKnights updatedBitboards
                   `bitboardUnion` whiteRooks updatedBitboards
                   `bitboardUnion` whiteQueens updatedBitboards
                   `bitboardUnion` whiteKings updatedBitboards
  blackOccupancy = blackPawns updatedBitboards
                   `bitboardUnion` blackBishops updatedBitboards
                   `bitboardUnion` blackKnights updatedBitboards
                   `bitboardUnion` blackRooks updatedBitboards
                   `bitboardUnion` blackQueens updatedBitboards
                   `bitboardUnion` blackKings updatedBitboards
  updatedBitboards = addPieceTo
    (removePieceFrom bitboards promotedPiece from)
    (Just p)
    to

  promotedPiece    = bitboardSpecificPieceAt bitboards from Pawn

bitboardMovePieceCapture
  :: BitboardRepresentation -> Coordinate -> Coordinate -> BitboardRepresentation
bitboardMovePieceCapture bitboards from to =
  updatedBitboards
    { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
    , whiteOccupancy = whiteOccupancy
    , blackOccupancy = blackOccupancy
    }

 where
  whiteOccupancy = whitePawns updatedBitboards
                       `bitboardUnion` whiteBishops updatedBitboards
                       `bitboardUnion` whiteKnights updatedBitboards
                       `bitboardUnion` whiteRooks updatedBitboards
                       `bitboardUnion` whiteQueens updatedBitboards
                       `bitboardUnion` whiteKings updatedBitboards
  blackOccupancy = blackPawns updatedBitboards
                       `bitboardUnion` blackBishops updatedBitboards
                       `bitboardUnion` blackKnights updatedBitboards
                       `bitboardUnion` blackRooks updatedBitboards
                       `bitboardUnion` blackQueens updatedBitboards
                       `bitboardUnion` blackKings updatedBitboards
  updatedBitboards = addPieceTo
    (removePieceFrom (removePieceFrom bitboards capturedPiece to)
                     movedPiece
                     from
    )
    movedPiece
    to

  capturedPiece = bitboardPieceAt bitboards to
  movedPiece    = bitboardPieceAt bitboards from

bitboardMovePieceStandard
  :: BitboardRepresentation -> Coordinate -> Coordinate -> BitboardRepresentation
bitboardMovePieceStandard bitboards from to =
  updatedBitboards
    { totalOccupancy = whiteOccupancy `bitboardUnion` blackOccupancy
    , whiteOccupancy = whiteOccupancy
    , blackOccupancy = blackOccupancy
    }

 where
  whiteOccupancy = whitePawns updatedBitboards
                   `bitboardUnion` whiteBishops updatedBitboards
                   `bitboardUnion` whiteKnights updatedBitboards
                   `bitboardUnion` whiteRooks updatedBitboards
                   `bitboardUnion` whiteQueens updatedBitboards
                   `bitboardUnion` whiteKings updatedBitboards
  blackOccupancy = blackPawns updatedBitboards
                   `bitboardUnion` blackBishops updatedBitboards
                   `bitboardUnion` blackKnights updatedBitboards
                   `bitboardUnion` blackRooks updatedBitboards
                   `bitboardUnion` blackQueens updatedBitboards
                   `bitboardUnion` blackKings updatedBitboards
  updatedBitboards =
    addPieceTo (removePieceFrom bitboards movedPiece from) movedPiece to
  movedPiece = bitboardPieceAt bitboards from

bitboardOwnerAt :: BitboardRepresentation -> Coordinate -> Maybe Player
bitboardOwnerAt b c | isOccupied (whiteOccupancy b) c = Just White
                    | isOccupied (blackOccupancy b) c = Just Black
                    | otherwise = Nothing

bitboardPieceAt :: BitboardRepresentation -> Coordinate -> Maybe Piece
bitboardPieceAt b c | isOccupied (whitePawns b) c   = Just $ Piece Pawn White
                    | isOccupied (blackPawns b) c   = Just $ Piece Pawn Black
                    | isOccupied (whiteBishops b) c = Just $ Piece Bishop White
                    | isOccupied (blackBishops b) c = Just $ Piece Bishop Black
                    | isOccupied (whiteKnights b) c = Just $ Piece Knight White
                    | isOccupied (blackKnights b) c = Just $ Piece Knight Black
                    | isOccupied (whiteRooks b) c   = Just $ Piece Rook White
                    | isOccupied (blackRooks b) c   = Just $ Piece Rook Black
                    | isOccupied (whiteQueens b) c  = Just $ Piece Queen White
                    | isOccupied (blackQueens b) c  = Just $ Piece Queen Black
                    | isOccupied (whiteKings b) c   = Just $ Piece King White
                    | isOccupied (blackKings b) c   = Just $ Piece King Black
                    | otherwise                     = Nothing

bitboardSpecificPieceAt :: BitboardRepresentation -> Coordinate -> PieceType -> Maybe Piece
bitboardSpecificPieceAt b c Pawn   | (isOccupied (whitePawns b) c) = Just $ Piece Pawn White
bitboardSpecificPieceAt b c Pawn   | isOccupied (blackPawns b) c = Just $ Piece Pawn Black
bitboardSpecificPieceAt b c Bishop | isOccupied (whiteBishops b) c = Just $ Piece Bishop White
bitboardSpecificPieceAt b c Bishop | isOccupied (blackBishops b) c = Just $ Piece Bishop Black
bitboardSpecificPieceAt b c Knight | isOccupied (whiteKnights b) c = Just $ Piece Knight White
bitboardSpecificPieceAt b c Knight | isOccupied (blackKnights b) c = Just $ Piece Knight Black
bitboardSpecificPieceAt b c Rook   | isOccupied (whiteRooks b) c = Just $ Piece Rook White
bitboardSpecificPieceAt b c Rook   | isOccupied (blackRooks b) c = Just $ Piece Rook Black
bitboardSpecificPieceAt b c Queen  | isOccupied (whiteQueens b) c = Just $ Piece Queen White
bitboardSpecificPieceAt b c Queen  | isOccupied (blackQueens b) c = Just $ Piece Queen Black
bitboardSpecificPieceAt b c King   | isOccupied (whiteKings b) c = Just $ Piece King White
bitboardSpecificPieceAt b c King   | isOccupied (blackKings b) c = Just $ Piece King Black
bitboardSpecificPieceAt _ _ _      = Nothing

occupiedCoordinates :: BitboardRepresentation -> [Coordinate]
occupiedCoordinates bitboards = filter (bitboardIsOccupied bitboards)
  $ map (indicesToCoordinate . squareIndexToIndices) [0 .. 63]

bitboardIsOccupied :: BitboardRepresentation -> Coordinate -> Bool
bitboardIsOccupied bitboard c | isOccupied (totalOccupancy bitboard) c = True
                              | otherwise                              = False

translateNorth :: Bitboard -> Bitboard
translateNorth (Bitboard b) = Bitboard $ rotateL b 8

rankMask :: Int -> Bitboard
rankMask r = Bitboard $ shiftL 255 ((r * 8) .&. 56)

fileMask :: Int -> Bitboard
fileMask f = Bitboard $ shiftL 72340172838076673 (f .&. 7) where

diagonalMask :: Int -> Bitboard
diagonalMask diagonal = Bitboard
  $ shiftL (shiftR 9241421688590303745 south) north where
  diag  = (-8) * diagonal
  north = ((-1) * diag) .&. (shiftR diag 31)
  south = diag .&. (shiftR ((-1) * diag) 31)

antiDiagonalMask :: Int -> Bitboard
antiDiagonalMask antiDiagonal = Bitboard
  $ shiftL (shiftR 72624976668147840 south) north where
  diag  = 56 - 8 * antiDiagonal
  north = ((-1) * diag) .&. (shiftR diag 31)
  south = diag .&. (shiftR ((-1) * diag) 31)

northRay :: (Int T.:!: Int) -> Bitboard
northRay (rank T.:!: file) = positiveRayFromLine (fileMask file) (rank T.:!: file)

eastRay :: (Int T.:!: Int) -> Bitboard
eastRay (rank T.:!: file) = positiveRayFromLine (rankMask rank) (rank T.:!: file)

southRay :: (Int T.:!: Int) -> Bitboard
southRay (rank T.:!: file) = negativeRayFromLine (fileMask file) (rank T.:!: file)

westRay :: (Int T.:!: Int) -> Bitboard
westRay (rank T.:!: file) = negativeRayFromLine (rankMask rank) (rank T.:!: file)

northEastRay :: (Int T.:!: Int) -> Bitboard
northEastRay (rank T.:!: file) = positiveRayFromLine (diagonalMask $ rank - file) (rank T.:!: file)

southEastRay :: (Int T.:!: Int) -> Bitboard
southEastRay (rank T.:!: file) = negativeRayFromLine (antiDiagonalMask $ rank + file) (rank T.:!: file)

southWestRay :: (Int T.:!: Int) -> Bitboard
southWestRay (rank T.:!: file) = negativeRayFromLine (diagonalMask $ rank - file) (rank T.:!: file)

northWestRay :: (Int T.:!: Int) -> Bitboard
northWestRay (rank T.:!: file) = positiveRayFromLine (antiDiagonalMask $ rank + file) (rank T.:!: file)

positiveRayFromLine :: Bitboard -> (Int T.:!: Int) -> Bitboard
positiveRayFromLine (Bitboard bits) (rank T.:!: file) = Bitboard $ bits .&. (shiftL (-2) (indicesToSquareIndex (rank T.:!: file)))

negativeRayFromLine :: Bitboard -> (Int T.:!: Int) -> Bitboard
negativeRayFromLine (Bitboard bits) (rank T.:!: file) = Bitboard $ bits .&. ((shiftL 1 (indicesToSquareIndex (rank T.:!: file))) - 1)

bitscanForwardTable :: [Int]
bitscanForwardTable =
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
bitscanForward (Bitboard bits) = bitscanForwardTable !! (fromEnum lookupIndex)
  where lookupIndex = bits .&. ((-1) * bits) `mod` 67

bitscanReverseTable :: [Int]
bitscanReverseTable =
 [ 0
 , 0
 , 1, 1
 , 2, 2, 2, 2
 , 3, 3, 3, 3, 3, 3, 3, 3
 , 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
 , 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
 , 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6
 , 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
 ]

bitscanReverse :: Bitboard -> Int
bitscanReverse bitboard = bitscanReverse' 0 bitboard

bitscanReverse' :: Int -> Bitboard -> Int
bitscanReverse' acc (Bitboard bits) | bits > 0xFFFFFFFF = bitscanReverse' (acc + 32) . Bitboard $ bits `shiftR` 32
                                    | bits > 0xFFFF = bitscanReverse' (acc + 16) . Bitboard $ bits `shiftR` 16
                                    | bits > 0xFF = bitscanReverse' (acc + 8) . Bitboard $ bits `shiftR` 8
                                    | otherwise = acc + bitscanReverseTable !! (fromEnum bits)
