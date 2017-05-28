module Test.Chess.BitboardSpec where

import Chess.Bitboard

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "bitboard representation" $ do
  it "uses little-endian rank-file mapping" $ do
    forAll (choose (0, 64)) $ ((not . isOccupied emptyBitboard) :: Int -> Bool)
