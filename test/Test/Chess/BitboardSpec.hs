module Test.Chess.BitboardSpec where

import Chess.Bitboard

import Data.Int
import Data.Word

import Test.Hspec
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
      show (Bitboard 9820426766351346249) `shouldBe` ". . . 1 . . . 1\n1 . . 1 . . 1 .\n. 1 . 1 . 1 . .\n. . 1 1 1 . . .\n1 1 1 . 1 1 1 1\n. . 1 1 1 . . .\n. 1 . 1 . 1 . .\n1 . . 1 . . 1 .\n"

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
      let allMoves       = Bitboard 945066513
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








