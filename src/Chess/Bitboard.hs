{-# LANGUAGE FlexibleInstances #-}

module Chess.Bitboard
  ( Bitboard(..)
  , BitboardRepresentation(..)
  , bitboardIntersect
  , bitboardUnion
  , bitboardComplement
  , bitboardXOR
  , bitboardPieceAt
  , bitboardIsOccupied
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
  , regularGameToBitboardGame
  , bitboardMovePiece

  , whiteOccupancyFor
  , blackOccupancyFor
  , totalOccupancyFor
  , occupiedCoordinates

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
  , bitscanReverse
  ) where

import Chess.Base

import Data.Bits
import Data.Functor
import Data.List
import Data.Maybe
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
  { whitePawns            :: Bitboard
  , blackPawns            :: Bitboard
  , whiteBishops          :: Bitboard
  , blackBishops          :: Bitboard
  , whiteKnights          :: Bitboard
  , blackKnights          :: Bitboard
  , whiteRooks            :: Bitboard
  , blackRooks            :: Bitboard
  , whiteQueens           :: Bitboard
  , blackQueens           :: Bitboard
  , whiteKings            :: Bitboard
  , blackKings            :: Bitboard
  , totalOccupancy        :: Bitboard
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
bitboardToCoordinates (Bitboard bits) = map (indicesToCoordinate . squareIndexToIndices) . filter (\i -> testBit bits i) $ [0..63]

emptyBitboard :: Bitboard
emptyBitboard = Bitboard 0

bitboardComplement :: Bitboard -> Bitboard
bitboardComplement (Bitboard bits) = Bitboard $ complement bits

bitboardIntersect                             :: Bitboard -> Bitboard -> Bitboard
bitboardIntersect (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..&. b2

bitboardUnion                             :: Bitboard -> Bitboard -> Bitboard
bitboardUnion (Bitboard b1) (Bitboard b2) = Bitboard $ b1 Data.Bits..|. b2

bitboardXOR :: Bitboard -> Bitboard -> Bitboard
bitboardXOR (Bitboard b1) (Bitboard b2) = Bitboard $ b1 `Data.Bits.xor` b2

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

whiteOccupancyFor :: RegularBoardRepresentation -> Bitboard
whiteOccupancyFor b = whitePawns bitboard
  `bitboardUnion` whiteBishops bitboard
  `bitboardUnion` whiteKnights bitboard
  `bitboardUnion` whiteRooks bitboard
  `bitboardUnion` whiteQueens bitboard
  `bitboardUnion` whiteKings bitboard
    where bitboard = regularToBitboard b

blackOccupancyFor :: RegularBoardRepresentation -> Bitboard
blackOccupancyFor b = blackPawns bitboard
  `bitboardUnion` blackBishops bitboard
  `bitboardUnion` blackKnights bitboard
  `bitboardUnion` blackRooks bitboard
  `bitboardUnion` blackQueens bitboard
  `bitboardUnion` blackKings bitboard
    where bitboard = regularToBitboard b

totalOccupancyFor :: RegularBoardRepresentation -> Bitboard
totalOccupancyFor = foldr occupiedSquareToBitboard emptyBitboard . filter (isJust . pieceOn) . concat
  where occupiedSquareToBitboard occupiedSquare acc = Bitboard (shiftL 1 (indicesToSquareIndex . coordinateToIndices $ location occupiedSquare)) `bitboardUnion` acc

regularToBitboard   :: RegularBoardRepresentation -> BitboardRepresentation
regularToBitboard b = withoutTotalOccupancy { totalOccupancy = whitePawns withoutTotalOccupancy
      `bitboardUnion` whiteBishops withoutTotalOccupancy
      `bitboardUnion` whiteKnights withoutTotalOccupancy
      `bitboardUnion` whiteRooks withoutTotalOccupancy
      `bitboardUnion` whiteQueens withoutTotalOccupancy
      `bitboardUnion` whiteKings withoutTotalOccupancy
      `bitboardUnion` blackPawns withoutTotalOccupancy
      `bitboardUnion` blackBishops withoutTotalOccupancy
      `bitboardUnion` blackKnights withoutTotalOccupancy
      `bitboardUnion` blackRooks withoutTotalOccupancy
      `bitboardUnion` blackQueens withoutTotalOccupancy
      `bitboardUnion` blackKings withoutTotalOccupancy }

  where addPieceToBitboard square bitboards | pieceOn square == Just (Piece Pawn White) = bitboards { whitePawns = (whitePawns bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Pawn Black) = bitboards { blackPawns = (blackPawns bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Knight White) = bitboards { whiteKnights = (whiteKnights bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Knight Black) = bitboards { blackKnights = (blackKnights bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Bishop White) = bitboards { whiteBishops = (whiteBishops bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Bishop Black) = bitboards { blackBishops = (blackBishops bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Rook White) = bitboards { whiteRooks = (whiteRooks bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Rook Black) = bitboards { blackRooks = (blackRooks bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Queen White) = bitboards { whiteQueens = (whiteQueens bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece Queen Black) = bitboards { blackQueens = (blackQueens bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece King White) = bitboards { whiteKings = (whiteKings bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | pieceOn square == Just (Piece King Black) = bitboards { blackKings = (blackKings bitboards) `bitboardUnion` (Bitboard $ squareToBitboard square) }
                                            | otherwise = bitboards
        empty = (BitboardRepresentation
                  { whitePawns   = emptyBitboard
                  , blackPawns   = emptyBitboard
                  , whiteBishops = emptyBitboard
                  , blackBishops = emptyBitboard
                  , whiteKnights = emptyBitboard
                  , blackKnights = emptyBitboard
                  , whiteRooks   = emptyBitboard
                  , blackRooks   = emptyBitboard
                  , whiteQueens  = emptyBitboard
                  , blackQueens  = emptyBitboard
                  , whiteKings   = emptyBitboard
                  , blackKings   = emptyBitboard
                  , totalOccupancy        = emptyBitboard
                  })
        withoutTotalOccupancy = foldr addPieceToBitboard empty $ concat b
        squareToBitboard square = shiftL 1 (indicesToSquareIndex . coordinateToIndices $ location square)

regularGameToBitboardGame :: RegularGame -> Game BitboardRepresentation
regularGameToBitboardGame regularGame = Game {
      placement = regularToBitboard $ placement regularGame
    , activeColor = activeColor regularGame
    , castlingRights = castlingRights regularGame
    , enPassantSquare = enPassantSquare regularGame
    , halfMoveClock = halfMoveClock regularGame
    , fullMoveNumber = fullMoveNumber regularGame
  }

bitboardMovePiece :: BitboardRepresentation -> Move -> BitboardRepresentation
bitboardMovePiece bitboards move@(Move from to Capture _) = bitboardMovePieceCapture bitboards move
bitboardMovePiece bitboards move@(Move from to _ _) = bitboardMovePieceStandard bitboards move

bitboardMovePieceCapture :: BitboardRepresentation -> Move -> BitboardRepresentation
bitboardMovePieceCapture bitboards (Move { moveFrom = from, moveTo = to }) = updatedBitboards { totalOccupancy = whitePawns updatedBitboards
      `bitboardUnion` whiteBishops updatedBitboards
      `bitboardUnion` whiteKnights updatedBitboards
      `bitboardUnion` whiteRooks updatedBitboards
      `bitboardUnion` whiteQueens updatedBitboards
      `bitboardUnion` whiteKings updatedBitboards
      `bitboardUnion` blackPawns updatedBitboards
      `bitboardUnion` blackBishops updatedBitboards
      `bitboardUnion` blackKnights updatedBitboards
      `bitboardUnion` blackRooks updatedBitboards
      `bitboardUnion` blackQueens updatedBitboards
      `bitboardUnion` blackKings updatedBitboards }

  where removePieceFrom :: BitboardRepresentation -> Maybe Piece -> Coordinate -> BitboardRepresentation
        removePieceFrom bitboards piece coord | piece == Just (Piece Pawn White) = bitboards { whitePawns = (whitePawns bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Pawn Black) = bitboards { blackPawns = (blackPawns bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Knight White) = bitboards { whiteKnights = (whiteKnights bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Knight Black) = bitboards { blackKnights = (blackKnights bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Bishop White) = bitboards { whiteBishops = (whiteBishops bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Bishop Black) = bitboards { blackBishops = (blackBishops bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Rook White) = bitboards { whiteRooks = (whiteRooks bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Rook Black) = bitboards { blackRooks = (blackRooks bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Queen White) = bitboards { whiteQueens = (whiteQueens bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece Queen Black) = bitboards { blackQueens = (blackQueens bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece King White) = bitboards { whiteKings = (whiteKings bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | piece == Just (Piece King Black) = bitboards { blackKings = (blackKings bitboards) `bitboardIntersect` (singleVacancy coord) }
                                              | otherwise = bitboards

        addPieceTo :: BitboardRepresentation -> Coordinate -> BitboardRepresentation
        addPieceTo bitboards coord | movedPiece == Just (Piece Pawn White) = bitboards { whitePawns = (whitePawns bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Pawn Black) = bitboards { blackPawns = (blackPawns bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Knight White) = bitboards { whiteKnights = (whiteKnights bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Knight Black) = bitboards { blackKnights = (blackKnights bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Bishop White) = bitboards { whiteBishops = (whiteBishops bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Bishop Black) = bitboards { blackBishops = (blackBishops bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Rook White) = bitboards { whiteRooks = (whiteRooks bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Rook Black) = bitboards { blackRooks = (blackRooks bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Queen White) = bitboards { whiteQueens = (whiteQueens bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Queen Black) = bitboards { blackQueens = (blackQueens bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece King White) = bitboards { whiteKings = (whiteKings bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece King Black) = bitboards { blackKings = (blackKings bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | otherwise = bitboards

        updatedBitboards = addPieceTo (removePieceFrom (removePieceFrom bitboards capturedPiece to) movedPiece from) to

        singleOccupant coord = Bitboard $ shiftL 1 (squareIndex coord)
        singleVacancy coord = bitboardComplement (singleOccupant coord)

        capturedPiece = bitboardPieceAt bitboards to
        movedPiece = bitboardPieceAt bitboards from

        squareIndex :: Coordinate -> Int
        squareIndex = indicesToSquareIndex . coordinateToIndices

bitboardMovePieceStandard :: BitboardRepresentation -> Move -> BitboardRepresentation
bitboardMovePieceStandard bitboards (Move { moveFrom = from, moveTo = to }) = updatedBitboards { totalOccupancy = whitePawns updatedBitboards
      `bitboardUnion` whiteBishops updatedBitboards
      `bitboardUnion` whiteKnights updatedBitboards
      `bitboardUnion` whiteRooks updatedBitboards
      `bitboardUnion` whiteQueens updatedBitboards
      `bitboardUnion` whiteKings updatedBitboards
      `bitboardUnion` blackPawns updatedBitboards
      `bitboardUnion` blackBishops updatedBitboards
      `bitboardUnion` blackKnights updatedBitboards
      `bitboardUnion` blackRooks updatedBitboards
      `bitboardUnion` blackQueens updatedBitboards
      `bitboardUnion` blackKings updatedBitboards }

  where removePieceFrom :: BitboardRepresentation -> Coordinate -> BitboardRepresentation
        removePieceFrom bitboards coord | movedPiece == Just (Piece Pawn White) = bitboards { whitePawns = (whitePawns bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Pawn Black) = bitboards { blackPawns = (blackPawns bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Knight White) = bitboards { whiteKnights = (whiteKnights bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Knight Black) = bitboards { blackKnights = (blackKnights bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Bishop White) = bitboards { whiteBishops = (whiteBishops bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Bishop Black) = bitboards { blackBishops = (blackBishops bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Rook White) = bitboards { whiteRooks = (whiteRooks bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Rook Black) = bitboards { blackRooks = (blackRooks bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Queen White) = bitboards { whiteQueens = (whiteQueens bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece Queen Black) = bitboards { blackQueens = (blackQueens bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece King White) = bitboards { whiteKings = (whiteKings bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | movedPiece == Just (Piece King Black) = bitboards { blackKings = (blackKings bitboards) `bitboardIntersect` (singleVacancy coord) }
                                        | otherwise = bitboards

        addPieceTo :: BitboardRepresentation -> Coordinate -> BitboardRepresentation
        addPieceTo bitboards coord | movedPiece == Just (Piece Pawn White) = bitboards { whitePawns = (whitePawns bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Pawn Black) = bitboards { blackPawns = (blackPawns bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Knight White) = bitboards { whiteKnights = (whiteKnights bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Knight Black) = bitboards { blackKnights = (blackKnights bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Bishop White) = bitboards { whiteBishops = (whiteBishops bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Bishop Black) = bitboards { blackBishops = (blackBishops bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Rook White) = bitboards { whiteRooks = (whiteRooks bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Rook Black) = bitboards { blackRooks = (blackRooks bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Queen White) = bitboards { whiteQueens = (whiteQueens bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece Queen Black) = bitboards { blackQueens = (blackQueens bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece King White) = bitboards { whiteKings = (whiteKings bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | movedPiece == Just (Piece King Black) = bitboards { blackKings = (blackKings bitboards) `bitboardUnion` (singleOccupant coord) }
                                   | otherwise = bitboards

        updatedBitboards = addPieceTo (removePieceFrom bitboards from) to

        singleOccupant coord = Bitboard $ shiftL 1 (squareIndex coord)
        singleVacancy coord = bitboardComplement (singleOccupant coord)

        movedPiece = bitboardPieceAt bitboards from

        squareIndex :: Coordinate -> Int
        squareIndex = indicesToSquareIndex . coordinateToIndices

bitboardPieceAt :: BitboardRepresentation -> Coordinate -> Maybe Piece
bitboardPieceAt b c | isOccupied (whitePawns b) c = Just $ Piece Pawn White
                    | isOccupied (blackPawns b) c = Just $ Piece Pawn Black
                    | isOccupied (whiteBishops b) c = Just $ Piece Bishop White
                    | isOccupied (blackBishops b) c = Just $ Piece Bishop Black
                    | isOccupied (whiteKnights b) c = Just $ Piece Knight White
                    | isOccupied (blackKnights b) c = Just $ Piece Knight Black
                    | isOccupied (whiteRooks b) c = Just $ Piece Rook White
                    | isOccupied (blackRooks b) c = Just $ Piece Rook Black
                    | isOccupied (whiteQueens b) c = Just $ Piece Queen White
                    | isOccupied (blackQueens b) c = Just $ Piece Queen Black
                    | isOccupied (whiteKings b) c = Just $ Piece King White
                    | isOccupied (blackKings b) c = Just $ Piece King Black
                    | otherwise = Nothing

occupiedCoordinates :: BitboardRepresentation -> [Coordinate]
occupiedCoordinates bitboards = filter (bitboardIsOccupied bitboards) $ map (indicesToCoordinate . squareIndexToIndices) [0..63]

bitboardIsOccupied :: BitboardRepresentation -> Coordinate -> Bool
bitboardIsOccupied bitboard c | isOccupied (totalOccupancy bitboard) c = True
                              | otherwise = False

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
