module Test.Chess.GameSpec where

import Chess.Base
import Chess.Bitboard
import Chess.FenParser
import Chess.Game
import Chess.Predicates

import Control.Monad.State.Lazy

import Data.Maybe

import Test.Hspec
import Test.Placements
import Test.Placements.King
import Test.Placements.Pawn

spec :: Spec
spec = do
  describe "makeMove" $ do
    context "legal moves" $ do
      it "accepts standard moves and updates the game's positional state" $
        doMakeMove startingPos (Move (Coordinate 'e' 2) (Coordinate 'e' 4)) `shouldBe` kingOpening

      it "accepts captures and removes the captured piece from the board" $ do
        let fromPosition = regularGameToBitboardGame (setupGame [ (Piece Pawn White, Coordinate 'c' 4)
                                      , (Piece Pawn Black, Coordinate 'd' 5)
                                      , (Piece King White, Coordinate 'e' 1)
                                      , (Piece King Black, Coordinate 'e' 8)
                                      ])
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece Pawn Black, Coordinate 'c' 4)
                     , (Piece King White, Coordinate 'e' 1)
                     , (Piece King Black, Coordinate 'e' 8)])
        doMakeMove fromPosition { activeColor = Black } (Capture (Coordinate 'd' 5) (Coordinate 'c' 4)) `shouldBe`
           expectedPosition { activeColor = White }

      it "accepts white kingside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'g' 1)
                              , (Piece Rook White, Coordinate 'f' 1)
                              ])
        doMakeMove (regularGameToBitboardGame whiteKingOOTest) (Castle (Coordinate 'e' 1) (Coordinate 'g' 1)) `shouldBe`
           expectedPosition { activeColor = Black
                            , castlingRights = CastleRights False True False True
                            }

      it "accepts white queenside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'c' 1)
                                          , (Piece Rook White, Coordinate 'd' 1)
                                          ])
        doMakeMove (regularGameToBitboardGame whiteKingOOOTest) (Castle (Coordinate 'e' 1) (Coordinate 'c' 1)) `shouldBe`
           expectedPosition { activeColor = Black
                            , castlingRights = CastleRights False True False True
                            }

      it "accepts black kingside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece King Black, Coordinate 'g' 8)
                     , (Piece Rook Black, Coordinate 'f' 8)
                     ])
        doMakeMove (regularGameToBitboardGame blackKingOOTest) (Castle (Coordinate 'e' 8) (Coordinate 'g' 8)) `shouldBe`
           expectedPosition { activeColor = White
                        , castlingRights = CastleRights True False True False
                        }

      it "accepts black queenside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece King Black, Coordinate 'c' 8)
                     , (Piece Rook Black, Coordinate 'd' 8)
                     ])
        doMakeMove (regularGameToBitboardGame blackKingOOOTest) (Castle (Coordinate 'e' 8) (Coordinate 'c' 8)) `shouldBe`
           expectedPosition { activeColor = White
                        , castlingRights = CastleRights True False True False
                        }

      it "allows white to promote pawns" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece Queen White, Coordinate 'e' 8)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove whitePromotionTest (Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Queen White)) `shouldBe` expectedPosition { activeColor = Black }

      it "allows black to promote pawns" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece Queen Black, Coordinate 'e' 1)
                       , (Piece King White, Coordinate 'a' 8)
                       , (Piece King Black, Coordinate 'h' 8)])
        doMakeMove blackPromotionTest (Promote (Coordinate 'e' 2) (Coordinate 'e' 1) (Piece Queen Black))
          `shouldBe` expectedPosition { activeColor = White }

      it "allows white to en passant" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece Pawn White, Coordinate 'd' 6)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove (regularGameToBitboardGame $ whiteEnPassantTest { activeColor = White }) (EnPassant (Coordinate 'e' 5) (Coordinate 'd' 6)) `shouldBe` expectedPosition { activeColor = Black }

      it "allows black to en passant" $ do
        let expectedPosition = regularGameToBitboardGame (setupGame [ (Piece Pawn Black, Coordinate 'e' 3)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove (regularGameToBitboardGame $ blackEnPassantTest { activeColor = Black }) (EnPassant (Coordinate 'd' 4) (Coordinate 'e' 3)) `shouldBe` expectedPosition { activeColor = White }

    context "illegal moves" $ do
      it "returns a value of false for illegal moves" $
        moveAccepted startingPos (Move (Coordinate 'd' 1) (Coordinate 'd' 8)) `shouldBe` False

      it "does not modify the game state for illegal moves" $
        doMakeMove startingPos (Move (Coordinate 'd' 1) (Coordinate 'd' 8)) `shouldBe` startingPos

      it "does not allow black to make a move during white's turn" $
        doMakeMove startingPos (Move (Coordinate 'd' 7) (Coordinate 'd' 5)) `shouldBe` startingPos

      it "does not allow white to make a move during black's turn" $
        doMakeMove kingOpening (Move (Coordinate 'd' 2) (Coordinate 'd' 4)) `shouldBe` kingOpening

      it "does not allow black to castle during white's turn" $ do
        let successful (Right x) = x
        let position = regularGameToBitboardGame . successful $ parseFen "" "rn2k1nr/p2p1ppp/8/2pp4/2b5/8/PB2NPPP/R3KB1R w KQkq - 0 1"
        let position' = fromJust . makeMoveFrom position $ Capture (Coordinate 'b' 2) (Coordinate 'g' 7)
        doMakeMove position' (Castle (Coordinate 'e' 1) (Coordinate 'c' 1)) `shouldBe` position'

      it "returns a value of false for en passant moves when none are allowed" $
        moveAccepted (regularGameToBitboardGame $ whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing }) (EnPassant (Coordinate 'e' 5) (Coordinate 'd' 6)) `shouldBe` False

      it "does not modify the game state for en passant moves when none are allowed" $
        doMakeMove (regularGameToBitboardGame $ whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing }) (EnPassant (Coordinate 'e' 5) (Coordinate 'd' 6)) `shouldBe` (regularGameToBitboardGame $ whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing })

      it "does not allow pawn promotion if the pawn is pinned to its king" $
        doMakeMove promotionPinTest (Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Queen White)) `shouldBe` promotionPinTest

      it "does not allow en passant if the capturing pawn is pinned to its king" $
        doMakeMove enPassantPinTest (EnPassant (Coordinate 'e' 5) (Coordinate 'd' 6)) `shouldBe` enPassantPinTest

      context "checks" $ do
        it "allows capturing a checking piece to get out of check" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Queen White, Coordinate 'd' 1)
                                        , (Piece Rook White, Coordinate 'a' 1)
                                        , (Piece Knight Black, Coordinate 'c' 2)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        ])
          moveAccepted fromPosition (Capture (Coordinate 'd' 1) (Coordinate 'c' 2)) `shouldBe` True

        it "does not allow the king to move into check by a queen" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                       , (Piece King Black, Coordinate 'e' 8)
                       , (Piece Queen Black, Coordinate 'd' 8)])
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1))
             `shouldBe` (regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                  , (Piece King Black, Coordinate 'e' 8)
                                  , (Piece Queen Black, Coordinate 'd' 8)
                                  ])

        it "returns false when the king moves into check by a queen" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Queen Black, Coordinate 'd' 8)])
          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` False

        it "does not allow the king to move into check by a rook" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Rook Black, Coordinate 'd' 8)])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Rook Black, Coordinate 'd' 8)
                                           ]
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a rook" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Rook Black, Coordinate 'd' 8)])
          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` False

        it "does not allow the king to move into check by a bishop" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Bishop Black, Coordinate 'h' 5)])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Bishop Black, Coordinate 'h' 5)
                                           ]
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a bishop" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Bishop Black, Coordinate 'h' 5)
                                        ])
          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` False

        it "does not allow the king to move into check by a knight" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                       , (Piece King Black, Coordinate 'e' 8)
                       , (Piece Knight Black, Coordinate 'e' 3)])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Knight Black, Coordinate 'e' 3)
                                           ]
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1))
             `shouldBe` expectedPosition

        it "returns false when the king moves into check by a knight" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Knight Black, Coordinate 'e' 3)
                                        ])
          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1))
             `shouldBe` False

        it "does not allow the king to move into check by a pawn" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Pawn Black, Coordinate 'e' 2)])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Pawn Black, Coordinate 'e' 2)
                                           ]
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a pawn" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Pawn Black, Coordinate 'e' 2)
                                        ])
          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1))
             `shouldBe` False

        it "does not allow the king to move into check by a king" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'c' 2)
                                        ])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'c' 2)
                                           ]
          doMakeMove fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a king" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'c' 2)
                                        ])

          moveAccepted fromPosition (Move (Coordinate 'e' 1) (Coordinate 'd' 1)) `shouldBe` False

        it "does not allow the king to be captured on the next move" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Pawn White, Coordinate 'a' 2)
                                        , (Piece Queen Black, Coordinate 'h' 1)
                                        , (Piece King Black, Coordinate 'e' 8)])
          let expectedPosition = regularGameToBitboardGame $ setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece Pawn White, Coordinate 'a' 2)
                                           , (Piece Queen Black, Coordinate 'h' 1)
                                           , (Piece King Black, Coordinate 'e' 8)]
          doMakeMove fromPosition (Move (Coordinate 'a' 2) (Coordinate 'a' 4)) `shouldBe` expectedPosition

        it "returns false if the king can be captured on the next move" $ do
          let fromPosition = regularGameToBitboardGame (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Pawn White, Coordinate 'a' 2)
                                        , (Piece Queen Black, Coordinate 'h' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        ])
          moveAccepted fromPosition (Move (Coordinate 'a' 2) (Coordinate 'a' 4))
             `shouldBe` False

        it "does not allow a piece pinned to the king to be moved" $
          doMakeMove discoveredCheckTest (Move (Coordinate 'e' 5) (Coordinate 'h' 5)) `shouldBe` discoveredCheckTest

        it "returns false if a piece pinned to the king is moved" $
          moveAccepted discoveredCheckTest (Move (Coordinate 'e' 5) (Coordinate 'h' 5)) `shouldBe` False

        it "requires the king to move under double check" $
          doMakeMove doubleCheckTest (Capture (Coordinate 'h' 7) (Coordinate 'e' 4)) `shouldBe` doubleCheckTest

        it "returns false if the king does not move under double check" $
          moveAccepted doubleCheckTest (Capture (Coordinate 'h' 7) (Coordinate 'e' 4)) `shouldBe` False

        it "does not allow the king to castle into check" $
          doMakeMove castleIntoCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'g' 1)) `shouldBe` castleIntoCheckTest

        it "returns false if the king castles into check" $
          moveAccepted castleIntoCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'g' 1)) `shouldBe` False

        it "does not allow the white king to kingside castle through check" $
          doMakeMove whiteKingsideCastleThroughCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'g' 1)) `shouldBe` whiteKingsideCastleThroughCheckTest

        it "returns false if the white king kingside castles through check" $
          moveAccepted whiteKingsideCastleThroughCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'g' 1)) `shouldBe` False

        it "does not allow the white king to queenside castle through check" $
          doMakeMove whiteQueensideCastleThroughCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'c' 1)) `shouldBe` whiteQueensideCastleThroughCheckTest

        it "returns false if the white king queenside castles through check" $
          moveAccepted whiteQueensideCastleThroughCheckTest (Castle (Coordinate 'e' 1) (Coordinate 'c' 1)) `shouldBe` False

        it "does not allow the black king to kingside castle through check" $
          doMakeMove blackKingsideCastleThroughCheckTest (Castle (Coordinate 'e' 8) (Coordinate 'g' 8)) `shouldBe` blackKingsideCastleThroughCheckTest

        it "returns false if the black king kingside castles through check" $
          moveAccepted blackKingsideCastleThroughCheckTest (Castle (Coordinate 'e' 8) (Coordinate 'g' 8)) `shouldBe` False

        it "does not allow the black king to queenside castle through check" $
          doMakeMove blackQueensideCastleThroughCheckTest (Castle (Coordinate 'e' 8) (Coordinate 'c' 8)) `shouldBe` blackQueensideCastleThroughCheckTest

        it "returns false if the black king queenside castles through check" $
          moveAccepted blackQueensideCastleThroughCheckTest (Castle (Coordinate 'e' 8) (Coordinate 'c' 8)) `shouldBe` False
