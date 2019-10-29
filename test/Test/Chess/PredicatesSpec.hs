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

    {--
    it "does not allow castling out of checkmate when the castling squares are attacked" $
      isCheckmate (regularGameToBitboardGame $ setupGame [ (Piece Bishop White, Coordinate 'a' 3)
                                                         , (Piece Queen White, Coordinate 'e' 7)
                                                         , (Piece King White, Coordinate 'e' 1)
                                                         , (Piece King Black, Coordinate 'e' 8)
                                                         , (Piece Rook Black, Coordinate 'a' 8)
                                                         , (Piece Rook Black, Coordinate 'h' 8)
                                                         ]) { activeColor = Black } Black `shouldBe` True
    --}

  describe "castling legality" $ do
    describe "kingside castling" $ do
      it "detects when the white kingside castle is safe" $ do
        let position = setupGame [ (Piece King White, Coordinate 'e' 1)
                                 , (Piece Rook White, Coordinate 'h' 1)
                                 , (Piece King Black, Coordinate 'e' 8)
                                 ]
        isKingsideCastleSafe (regularGameToBitboardGame position) White `shouldBe` True

      it "detects when the white kingside castle is under attack" $ do
        let f1Attacked = setupGame [ (Piece King White, Coordinate 'e' 1)
                                 , (Piece Rook White, Coordinate 'h' 1)
                                 , (Piece King Black, Coordinate 'e' 8)
                                 , (Piece Rook Black, Coordinate 'f' 8)
                                 ]

        let g1Attacked = setupGame [ (Piece King White, Coordinate 'e' 1)
                                 , (Piece Rook White, Coordinate 'h' 1)
                                 , (Piece King Black, Coordinate 'e' 8)
                                 , (Piece Rook Black, Coordinate 'g' 8)
                                 ]

        isKingsideCastleSafe (regularGameToBitboardGame f1Attacked) White `shouldBe` False
        isKingsideCastleSafe (regularGameToBitboardGame g1Attacked) White `shouldBe` False

      it "detects when the black kingside castle is safe" $ do
        let position = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                 , (Piece Rook Black, Coordinate 'h' 8)
                                 , (Piece King White, Coordinate 'e' 1)
                                 ]) { activeColor = Black }
        isKingsideCastleSafe (regularGameToBitboardGame position) Black `shouldBe` True

      it "detects when the white kingside castle is under attack" $ do
        let f8Attacked = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                 , (Piece Rook Black, Coordinate 'h' 8)
                                 , (Piece King White, Coordinate 'e' 1)
                                 , (Piece Rook White, Coordinate 'f' 1)
                                 ]) { activeColor = Black }

        let g8Attacked = (setupGame [ (Piece King White, Coordinate 'e' 8)
                                 , (Piece Rook White, Coordinate 'h' 8)
                                 , (Piece King Black, Coordinate 'e' 1)
                                 , (Piece Rook Black, Coordinate 'g' 1)
                                 ]) { activeColor = Black }

        isKingsideCastleSafe (regularGameToBitboardGame f8Attacked) Black `shouldBe` False
        isKingsideCastleSafe (regularGameToBitboardGame g8Attacked) Black `shouldBe` False

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
