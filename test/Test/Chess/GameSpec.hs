module Test.Chess.GameSpec where

import Chess.Base
import Chess.Game
import Chess.Predicates

import Control.Monad.State.Lazy

import Test.Hspec
import Test.Placements
import Test.Placements.King
import Test.Placements.Pawn

spec :: Spec
spec = do
  describe "makeMove" $ do
    context "legal moves" $ do
      it "accepts standard moves and updates the game's positional state" $
        doMakeMove startingPos (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 4, moveType = Standard }) `shouldBe` kingOpening

      it "accepts captures and removes the captured piece from the board" $ do
        let fromPosition = (setupGame [ (Piece Pawn White, Coordinate 'c' 4)
                                      , (Piece Pawn Black, Coordinate 'd' 5)
                                      , (Piece King White, Coordinate 'e' 1)
                                      , (Piece King Black, Coordinate 'e' 8)
                                      ])
        let expectedPosition = (setupGame [ (Piece Pawn Black, Coordinate 'c' 4)
                     , (Piece King White, Coordinate 'e' 1)
                     , (Piece King Black, Coordinate 'e' 8)])
        doMakeMove fromPosition { activeColor = Black } (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'c' 4, moveType = Capture }) `shouldBe`
           expectedPosition { activeColor = White }

      it "accepts white kingside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = (setupGame [ (Piece King White, Coordinate 'g' 1)
                              , (Piece Rook White, Coordinate 'f' 1)
                              ])
        doMakeMove whiteKingOOTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle }) `shouldBe`
           expectedPosition { activeColor = Black
                            , castlingRights = CastleRights False True False True
                            }

      it "accepts white queenside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = (setupGame [ (Piece King White, Coordinate 'c' 1)
                                          , (Piece Rook White, Coordinate 'd' 1)
                                          ])
        doMakeMove whiteKingOOOTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle }) `shouldBe`
           expectedPosition { activeColor = Black
                            , castlingRights = CastleRights False True False True
                            }

      it "accepts black kingside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = (setupGame [ (Piece King Black, Coordinate 'g' 8)
                     , (Piece Rook Black, Coordinate 'f' 8)
                     ])
        doMakeMove blackKingOOTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle }) `shouldBe`
           expectedPosition { activeColor = White
                        , castlingRights = CastleRights True False True False
                        }

      it "accepts black queenside castles, updating castling rights and moving the rook" $ do
        let expectedPosition = (setupGame [ (Piece King Black, Coordinate 'c' 8)
                     , (Piece Rook Black, Coordinate 'd' 8)
                     ])
        doMakeMove blackKingOOOTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle }) `shouldBe`
           expectedPosition { activeColor = White
                        , castlingRights = CastleRights True False True False
                        }

      it "allows white to promote pawns" $ do
        let expectedPosition = (setupGame [ (Piece Queen White, Coordinate 'e' 8)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove whitePromotionTest (Move { movePromoteTo = Just (Piece Queen White), moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion }) `shouldBe` expectedPosition { activeColor = Black }

      it "allows black to promote pawns" $ do
        let expectedPosition = (setupGame [ (Piece Queen Black, Coordinate 'e' 1)
                       , (Piece King White, Coordinate 'a' 8)
                       , (Piece King Black, Coordinate 'h' 8)])
        doMakeMove blackPromotionTest (Move { movePromoteTo = Just (Piece Queen Black), moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 1, moveType = Promotion })
          `shouldBe` expectedPosition { activeColor = White }

      it "allows white to en passant" $ do
        let expectedPosition = (setupGame [ (Piece Pawn White, Coordinate 'd' 6)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove whiteEnPassantTest { activeColor = White } (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = EnPassant }) `shouldBe` expectedPosition { activeColor = Black }

      it "allows black to en passant" $ do
        let expectedPosition = (setupGame [ (Piece Pawn Black, Coordinate 'e' 3)
                       , (Piece King White, Coordinate 'a' 1)
                       , (Piece King Black, Coordinate 'h' 1)])
        doMakeMove blackEnPassantTest { activeColor = Black } (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = EnPassant }) `shouldBe` expectedPosition { activeColor = White }

    context "illegal moves" $ do

      it "returns a value of false for illegal moves" $
        moveAccepted startingPos (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 1, moveTo = Coordinate 'd' 8, moveType = Standard }) `shouldBe` False

      it "does not modify the game state for illegal moves" $
        doMakeMove startingPos (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 1, moveTo = Coordinate 'd' 8, moveType = Standard }) `shouldBe` startingPos

      it "does not allow black to make a move during white's turn" $
        doMakeMove startingPos (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 7, moveTo = Coordinate 'd' 5, moveType = Standard }) `shouldBe` startingPos

      it "does not allow white to make a move during black's turn" $
        doMakeMove kingOpening (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 2, moveTo = Coordinate 'd' 4, moveType = Standard }) `shouldBe` kingOpening

      it "returns a value of false for en passant moves when none are allowed" $
        moveAccepted whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing } (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = EnPassant }) `shouldBe` False

      it "does not modify the game state for en passant moves when none are allowed" $
        doMakeMove whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing } (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = EnPassant }) `shouldBe` whiteEnPassantTest { activeColor = White, enPassantSquare = Nothing }

      it "does not allow pawn promotion if the pawn is pinned to its king" $
        doMakeMove promotionPinTest (Move { movePromoteTo = Just (Piece Queen White), moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion }) `shouldBe` promotionPinTest

      it "does not allow en passant if the capturing pawn is pinned to its king" $
        doMakeMove enPassantPinTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = EnPassant }) `shouldBe` enPassantPinTest

      context "checks" $ do
        it "allows capturing a checking piece to get out of check" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Queen White, Coordinate 'd' 1)
                                        , (Piece Rook White, Coordinate 'a' 1)
                                        , (Piece Knight Black, Coordinate 'c' 2)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        ])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'd' 1, moveTo = Coordinate 'c' 2, moveType = Capture }) `shouldBe` True

        it "does not allow the king to move into check by a queen" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                       , (Piece King Black, Coordinate 'e' 8)
                       , (Piece Queen Black, Coordinate 'd' 8)])
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard })
             `shouldBe` setupGame [ (Piece King White, Coordinate 'e' 1)
                                  , (Piece King Black, Coordinate 'e' 8)
                                  , (Piece Queen Black, Coordinate 'd' 8)
                                  ]

        it "returns false when the king moves into check by a queen" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Queen Black, Coordinate 'd' 8)])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` False

        it "does not allow the king to move into check by a rook" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Rook Black, Coordinate 'd' 8)])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Rook Black, Coordinate 'd' 8)
                                           ]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a rook" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Rook Black, Coordinate 'd' 8)])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` False

        it "does not allow the king to move into check by a bishop" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Bishop Black, Coordinate 'h' 5)])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Bishop Black, Coordinate 'h' 5)
                                           ]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a bishop" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Bishop Black, Coordinate 'h' 5)
                                        ])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` False

        it "does not allow the king to move into check by a knight" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                       , (Piece King Black, Coordinate 'e' 8)
                       , (Piece Knight Black, Coordinate 'e' 3)])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Knight Black, Coordinate 'e' 3)
                                           ]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard })
             `shouldBe` expectedPosition

        it "returns false when the king moves into check by a knight" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Knight Black, Coordinate 'e' 3)
                                        ])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard })
             `shouldBe` False

        it "does not allow the king to move into check by a pawn" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Pawn Black, Coordinate 'e' 2)])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'e' 8)
                                           , (Piece Pawn Black, Coordinate 'e' 2)
                                           ]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a pawn" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        , (Piece Pawn Black, Coordinate 'e' 2)
                                        ])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard })
             `shouldBe` False

        it "does not allow the king to move into check by a king" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'c' 2)
                                        ])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece King Black, Coordinate 'c' 2)
                                           ]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` expectedPosition

        it "returns false when the king moves into check by a king" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece King Black, Coordinate 'c' 2)
                                        ])

          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard }) `shouldBe` False

        it "does not allow the king to be captured on the next move" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Pawn White, Coordinate 'a' 2)
                                        , (Piece Queen Black, Coordinate 'h' 1)
                                        , (Piece King Black, Coordinate 'e' 8)])
          let expectedPosition = setupGame [ (Piece King White, Coordinate 'e' 1)
                                           , (Piece Pawn White, Coordinate 'a' 2)
                                           , (Piece Queen Black, Coordinate 'h' 1)
                                           , (Piece King Black, Coordinate 'e' 8)]
          doMakeMove fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'a' 2, moveTo = Coordinate 'a' 4, moveType = Standard }) `shouldBe` expectedPosition

        it "returns false if the king can be captured on the next move" $ do
          let fromPosition = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                        , (Piece Pawn White, Coordinate 'a' 2)
                                        , (Piece Queen Black, Coordinate 'h' 1)
                                        , (Piece King Black, Coordinate 'e' 8)
                                        ])
          moveAccepted fromPosition (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'a' 2, moveTo = Coordinate 'a' 4, moveType = Standard })
             `shouldBe` False

        it "does not allow a piece pinned to the king to be moved" $
          doMakeMove discoveredCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'h' 5, moveType = Standard }) `shouldBe` discoveredCheckTest

        it "returns false if a piece pinned to the king is moved" $
          moveAccepted discoveredCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'h' 5, moveType = Standard }) `shouldBe` False

        it "requires the king to move under double check" $
          doMakeMove doubleCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'h' 7, moveTo = Coordinate 'e' 4, moveType = Capture }) `shouldBe` doubleCheckTest

        it "returns false if the king does not move under double check" $
          moveAccepted doubleCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'h' 7, moveTo = Coordinate 'e' 4, moveType = Capture }) `shouldBe` False

        it "does not allow the king to castle into check" $
          doMakeMove castleIntoCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle }) `shouldBe` castleIntoCheckTest

        it "returns false if the king castles into check" $
          moveAccepted castleIntoCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle }) `shouldBe` False

        it "does not allow the white king to kingside castle through check" $
          doMakeMove whiteKingsideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle }) `shouldBe` whiteKingsideCastleThroughCheckTest

        it "returns false if the white king kingside castles through check" $
          moveAccepted whiteKingsideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle }) `shouldBe` False

        it "does not allow the white king to queenside castle through check" $
          doMakeMove whiteQueensideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle }) `shouldBe` whiteQueensideCastleThroughCheckTest

        it "returns false if the white king queenside castles through check" $
          moveAccepted whiteQueensideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle }) `shouldBe` False

        it "does not allow the black king to kingside castle through check" $
          doMakeMove blackKingsideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle }) `shouldBe` blackKingsideCastleThroughCheckTest

        it "returns false if the black king kingside castles through check" $
          moveAccepted blackKingsideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle }) `shouldBe` False

        it "does not allow the black king to queenside castle through check" $
          doMakeMove blackQueensideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle }) `shouldBe` blackQueensideCastleThroughCheckTest

        it "returns false if the black king queenside castles through check" $
          moveAccepted blackQueensideCastleThroughCheckTest (Move { movePromoteTo = Nothing, moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle }) `shouldBe` False
