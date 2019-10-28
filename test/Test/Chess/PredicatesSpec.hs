module Test.Chess.PredicatesSpec where

import Chess.Base
import Chess.Bitboard
import Chess.Predicates

import Test.Hspec
import Test.Placements

spec :: Spec
spec = do
  describe "isCheckmate" $ do
    it "accepts a game and a player, returning true if that player has been checkmated" $
      isCheckmate (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'a' 1)
                                                         , (Piece King Black, Coordinate 'e' 8)
                                                         , (Piece Queen Black, Coordinate 'h' 1)
                                                         , (Piece Rook Black, Coordinate 'h' 2)]) White `shouldBe` True

    it "accepts a game and a player, returning false if that player still has a legal move" $
      isCheckmate (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'a' 1)
                                                         , (Piece Queen White, Coordinate 'd' 3)
                                                         , (Piece King Black, Coordinate 'e' 8)
                                                         , (Piece Queen Black, Coordinate 'h' 1)
                                                         , (Piece Rook Black, Coordinate 'h' 2)]) White `shouldBe` False

    it "does not consider stalemated positions as checkmated positions" $
      isCheckmate (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'f' 7)
                                                         , (Piece Queen White, Coordinate 'g' 6)
                                                         , (Piece King Black, Coordinate 'h' 8)
                                                         ]) { activeColor = Black } Black `shouldBe` False

  describe "isStalemate" $ do
    it "accepts a game and a player, returning true if that player has been stalemated" $
      isStalemate (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'f' 7)
                                                         , (Piece Queen White, Coordinate 'g' 6)
                                                         , (Piece King Black, Coordinate 'h' 8)
                                                         ]) { activeColor = Black } Black `shouldBe` True

    it "accepts a game and a player, returning false if that player still has a legal move" $
      isStalemate (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'f' 7)
                                                         , (Piece Queen White, Coordinate 'g' 6)
                                                         , (Piece King Black, Coordinate 'h' 1)
                                                         ]) { activeColor = Black } Black `shouldBe` False
