{-# LANGUAGE OverloadedStrings #-}
module Test.Chess.FastFenParserSpec where

import Chess.Base
import Chess.Bitboard
import Chess.FastFenParser

import Test.Hspec
import Test.Placements

successful (Right x) = x

operaGame :: Game BitboardRepresentation
operaGame = Game
  { placement = BitboardRepresentation
    { whitePawns = Bitboard 268494592
    , blackPawns = Bitboard 63336336525885440
    , whiteBishops = Bitboard 274945015808
    , blackBishops = Bitboard 2305843009213693952
    , whiteKnights = Bitboard 8589934592
    , blackKnights = Bitboard 144150372447944704
    , whiteRooks = Bitboard 129
    , blackRooks = Bitboard 9295429630892703744
    , whiteQueens = Bitboard 131072
    , blackQueens = Bitboard 4503599627370496
    , whiteKings = Bitboard 16
    , blackKings = Bitboard 1152921504606846976
    , totalOccupancy = Bitboard 12966184737118021521
    , whiteOccupancy = Bitboard 283803576209
    , blackOccupancy = Bitboard 12966184453314445312
    }
  , activeColor     = Black
  , castlingRights  = (CastleRights True True True True)
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 10
  }

spec :: Spec
spec =
  describe "FEN string parsing" $ do
    it "parses the starting position correctly" $
      (successful $ fastParseFEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") `shouldBe` startingPos

    it "parses this position from the Opera Game" $
      (successful $ fastParseFEN "rn2kb1r/p3qppp/2p2n2/1N2p1B1/2B1P3/1Q6/PPP2PPP/R3K2R b KQkq - 0 10") `shouldBe` operaGame

    it "is the inverse of toFEN" $
      fastToFEN operaGame `shouldBe` "rn2kb1r/p3qppp/2p2n2/1N2p1B1/2B1P3/1Q6/PPP2PPP/R3K2R b KQkq - 0 10"
