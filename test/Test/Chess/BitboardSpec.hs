module Test.Chess.BitboardSpec where

import Chess.Bitboard

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
spec = describe "bitboard representation" $ do
  it "uses little-endian rank-file mapping" $ do
    forAll (choose (0, 64)) $ ((not . isOccupied emptyBitboard) :: Int -> Bool)

  it "has a squareIndex defined in terms of a rankIndex and a fileIndex" $ do
    forAll (bitboardsAnd rankAndFileIndices) $ (\(bitboard, (ri, fi)) -> (isOccupied bitboard (8 * ri + fi)) == isOccupied bitboard (ri, fi))
