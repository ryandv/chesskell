module Test.Chess.BitboardSpec where

import Chess.Base
import Chess.Bitboard

import Data.Int
import Data.Word

import Test.Hspec
import Test.Placements
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

rankAndFileIndices :: Gen (Int, Int)
rankAndFileIndices = do
  ri  <- choose (0, 7)
  fi  <- choose (0, 7)
  return (ri, fi)

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
  describe "formatting" $ do
    it "is showable" $ do
      show (Bitboard 9820426766351346249) `shouldBe` "\n. . . 1 . . . 1\n1 . . 1 . . 1 .\n. 1 . 1 . 1 . .\n. . 1 1 1 . . .\n1 1 1 . 1 1 1 1\n. . 1 1 1 . . .\n. 1 . 1 . 1 . .\n1 . . 1 . . 1 .\n"

  describe "representation" $ do
    it "uses little-endian rank-file mapping" $ do
      forAll (choose (0, 64)) $ ((not . isOccupied emptyBitboard) :: Int -> Bool)

    it "has a squareIndex defined in terms of a rankIndex and a fileIndex" $ do
      forAll (bitboardsAnd rankAndFileIndices) $ (\(bitboard, (ri, fi)) -> (isOccupied bitboard (8 * ri + fi)) == isOccupied bitboard (ri, fi))

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
      let allMoves       = Bitboard 10544115227674579473
      let enemyPieces    = Bitboard 65280
      let attackedPieces = Bitboard 37376
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

    1000001001000100001010000000000000101000010001001000001000000001
    --}

    it "include union" $ do
      let rookMoves   = Bitboard 269488144
      let bishopMoves = Bitboard 675578369
      let queenMoves  = Bitboard 945066513
      (rookMoves `bitboardUnion` bishopMoves) `shouldBe` queenMoves


  describe "conversion from regular board representations" $ do

    it "can produce an occupancy bitboard for white pawns" $ do
      whitePawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 71776119061217280

    it "can produce an occupancy bitboard for black pawns" $ do
      blackPawnOccupancyFor (placement startingPos) `shouldBe` Bitboard 65280

    it "can produce an occupancy bitboard for white knights" $ do
      whiteKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 4755801206503243776

    it "can produce an occupancy bitboard for black knights" $ do
      blackKnightOccupancyFor (placement startingPos) `shouldBe` Bitboard 66

    it "can produce an occupancy bitboard for white bishops" $ do
      whiteBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 2594073385365405696

    it "can produce an occupancy bitboard for black bishops" $ do
      blackBishopOccupancyFor (placement startingPos) `shouldBe` Bitboard 36

    it "can produce an occupancy bitboard for white rooks" $ do
      whiteRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 9295429630892703744

    it "can produce an occupancy bitboard for black rooks" $ do
      blackRookOccupancyFor (placement startingPos) `shouldBe` Bitboard 129

    it "can produce an occupancy bitboard for white queens" $ do
      whiteQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 1152921504606846976

    it "can produce an occupancy bitboard for black queens" $ do
      blackQueenOccupancyFor (placement startingPos) `shouldBe` Bitboard 16

    it "can produce an occupancy bitboard for white kings" $ do
      whiteKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 576460752303423488

    it "can produce an occupancy bitboard for black kings" $ do
      blackKingOccupancyFor (placement startingPos) `shouldBe` Bitboard 8

    it "can convert a RegularBoardRepresentation into a BitboardRepresentation" $ do
      regularToBitboard (placement startingPos) `shouldBe` BitboardRepresentation
        { whitePawns   = Bitboard 71776119061217280
        , blackPawns   = Bitboard 65280
        , whiteKnights = Bitboard 4755801206503243776
        , blackKnights = Bitboard 66
        , whiteBishops = Bitboard 2594073385365405696
        , blackBishops = Bitboard 36
        , whiteRooks   = Bitboard 9295429630892703744
        , blackRooks   = Bitboard 129
        , whiteQueens  = Bitboard 1152921504606846976
        , blackQueens  = Bitboard 16
        , whiteKings   = Bitboard 576460752303423488
        , blackKings   = Bitboard 8
        }
