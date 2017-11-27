module Test.Chess.BitboardSpec where

import Chess.Base
import Chess.Bitboard

import Data.Int
import Data.Word

import Test.Hspec
import Test.Placements
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.Util

rankAndFileIndices :: Gen (Int, Int)
rankAndFileIndices = do
  ri  <- rankIndices
  fi  <- fileIndices
  return (ri, fi)

rankIndices :: Gen Int
rankIndices = choose (0, 7)

fileIndices :: Gen Int
fileIndices = choose (0, 7)

files :: Gen File
files = choose ('a', 'h')

bitboards :: Gen Bitboard
bitboards = do
  w <- choose (minBound :: Word64, maxBound :: Word64)
  return $ Bitboard w

bitboardsAnd :: Gen a -> Gen (Bitboard, a)
bitboardsAnd gen = do
  bitboard <- bitboards
  a <- gen
  return (bitboard, a)


spec :: Spec
spec = describe "bitboard" $ do

  describe "integrations with Base" $ do
    it "can convert 0-based (rank, file) indices to Coordinates" $ do
      forAll coords (\c@(Coordinate f r) -> coordinateToIndices c == (r - 1, fromEnum f - 97))

  describe "formatting" $ do
    it "is showable" $ do
      show (Bitboard 9820426766351346249) `shouldBe` "\n. . . 1 . . . 1\n1 . . 1 . . 1 .\n. 1 . 1 . 1 . .\n. . 1 1 1 . . .\n1 1 1 . 1 1 1 1\n. . 1 1 1 . . .\n. 1 . 1 . 1 . .\n1 . . 1 . . 1 .\n"

  describe "representation" $ do
    {--
    . . . . . . . 1
    . . . . . . 1 .
    . . . . . 1 . .
    . . . . 1 . . .
    . . . 1 . . . .
    . . 1 . . . . .
    . 1 . . . . . .
    1 . . . . . . .
    --}
    it "uses little-endian rank-file mapping" $ do
      isOccupied (Bitboard 9241421688590303745) (0 :: Int) && isOccupied (Bitboard 9241421688590303745) (63 :: Int) `shouldBe` True

    it "has a squareIndex defined in terms of a rankIndex and a fileIndex" $ do
      forAll (bitboardsAnd rankAndFileIndices) $ (\(bitboard, (ri, fi)) -> (isOccupied bitboard $ 8 * ri + fi) == isOccupied bitboard (ri, fi))

    it "can be indexed by Coordinate" $ do
      forAll (bitboardsAnd coords) $ (\(bitboard, c@(Coordinate f r)) -> (isOccupied bitboard c) == isOccupied bitboard (coordinateToIndices c))

  describe "setwise operations" $ do
    {--
    . . . 1 . . . 1     . . . . . . . .     . . . . . . . .
    1 . . 1 . . 1 .     1 1 1 1 1 1 1 1     1 . . 1 . . 1 .
    . 1 . 1 . 1 . .     . . . . . . . .     . . . . . . . .
    . . 1 1 1 . . .     . . . . . . . .     . . . . . . . .
    1 1 1 * 1 1 1 1  &  . . . * . . . .  =  . . . * . . . .
    . . 1 1 1 . . .     . . . . . . . .     . . . . . . . .
    . 1 . 1 . 1 . .     . . . . . . . .     . . . . . . . .
    1 . . 1 . . 1 .     . . . . . . . .     . . . . . . . .
    --}

    it "include intersection" $ do
      let allMoves       = Bitboard 9820426766351346249
      let enemyPieces    = Bitboard 71776119061217280
      let attackedPieces = Bitboard 20547673299877888
      (allMoves `bitboardIntersect` enemyPieces) `shouldBe` attackedPieces

    {--
    . . . 1 . . . .     . . . . . . . 1     . . . 1 . . . 1
    . . . 1 . . . .     1 . . . . . 1 .     1 . . 1 . . 1 .
    . . . 1 . . . .     . 1 . . . 1 . .     . 1 . 1 . 1 . .
    . . . 1 . . . .     . . 1 . 1 . . .     . . 1 1 1 . . .
    1 1 1 * 1 1 1 1  |  . . . * . . . .  =  1 1 1 * 1 1 1 1
    . . . 1 . . . .     . . 1 . 1 . . .     . . 1 1 1 . . .
    . . . 1 . . . .     . 1 . . . 1 . .     . 1 . 1 . 1 . .
    . . . 1 . . . .     1 . . . . . 1 .     1 . . 1 . . 1 .

    0000100000001000000010000000100011110111000010000000100000001000
    --}

    it "include union" $ do
      let rookMoves   = Bitboard 578721386714368008
      let bishopMoves = Bitboard 9241705379636978241
      let queenMoves  = Bitboard 9820426766351346249
      (rookMoves `bitboardUnion` bishopMoves) `shouldBe` queenMoves

  describe "ray attacks" $ do
    it "can calculate ray attacks for any given rank" $ do
      forAll rankIndices $ (\r -> rankMask r == (Bitboard (255 * (256 ^ r))))

    it "can calculate ray attacks for any given file" $ do
      forAll files $ (\f -> fileMask f == (Bitboard (72340172838076673 * (2 ^ (fromEnum f - 97)))))

  describe "translations" $ do
    it "can translate bitboards in the north direction" $ do
      {--
      . . . 1 . . . 1     1 . . 1 . . 1 .
      1 . . 1 . . 1 .     . 1 . 1 . 1 . .
      . 1 . 1 . 1 . .     . . 1 1 1 . . .
      . . 1 1 1 . . .     1 1 1 * 1 1 1 1
      1 1 1 * 1 1 1 1 =>  . . 1 1 1 . . .
      . . 1 1 1 . . .     . 1 . 1 . 1 . .
      . 1 . 1 . 1 . .     1 . . 1 . . 1 .
      1 . . 1 . . 1 .     . . . 1 . . . 1
      --}

      translateNorth (Bitboard 9820426766351346249) `shouldBe` Bitboard 5272058161445620104

  describe "conversion from regular board representations" $ do

    it "can produce an occupancy bitboard for white pawns" $ do
      whitePawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 65280

    it "can produce an occupancy bitboard for black pawns" $ do
      blackPawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 71776119061217280

    it "can produce an occupancy bitboard for white knights" $ do
      whiteKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 66

    it "can produce an occupancy bitboard for black knights" $ do
      blackKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 4755801206503243776

    it "can produce an occupancy bitboard for white bishops" $ do
      whiteBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 36

    it "can produce an occupancy bitboard for black bishops" $ do
      blackBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 2594073385365405696

    it "can produce an occupancy bitboard for white rooks" $ do
      whiteRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 129

    it "can produce an occupancy bitboard for black rooks" $ do
      blackRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 9295429630892703744

    it "can produce an occupancy bitboard for white queens" $ do
      whiteQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 8

    it "can produce an occupancy bitboard for black queens" $ do
      blackQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 576460752303423488

    it "can produce an occupancy bitboard for white kings" $ do
      whiteKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 16

    it "can produce an occupancy bitboard for black kings" $ do
      blackKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 1152921504606846976

    it "can convert a RegularBoardRepresentation into a BitboardRepresentation" $ do
      regularToBitboard (placement startingPos) `shouldBe` BitboardRepresentation
        { whitePawns   = Bitboard 65280
        , blackPawns   = Bitboard 71776119061217280
        , whiteKnights = Bitboard 66
        , blackKnights = Bitboard 4755801206503243776
        , whiteBishops = Bitboard 36
        , blackBishops = Bitboard 2594073385365405696
        , whiteRooks   = Bitboard 129
        , blackRooks   = Bitboard 9295429630892703744
        , whiteQueens  = Bitboard 8
        , blackQueens  = Bitboard 576460752303423488
        , whiteKings   = Bitboard 16
        , blackKings   = Bitboard 1152921504606846976
        }
