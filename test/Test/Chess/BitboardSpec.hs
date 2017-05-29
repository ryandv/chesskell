module Test.Chess.BitboardSpec where

import Chess.Bitboard

import Test.Hspec
import Test.QuickCheck

rankAndFileIndices :: Gen (Int, Int)
rankAndFileIndices = do
  ri  <- choose (0, 7)
  fi  <- choose (0, 7)
  return (ri, fi)

spec :: Spec
spec = describe "bitboard representation" $ do
  it "uses little-endian rank-file mapping" $ do
    forAll (choose (0, 64)) $ ((not . isOccupied emptyBitboard) :: Int -> Bool)

  it "has a squareIndex defined in terms of a rankIndex and a fileIndex" $ do
    forAll rankAndFileIndices $ (\(ri, fi) -> (isOccupied emptyBitboard (8 * ri + fi)) == isOccupied emptyBitboard (ri, fi))
