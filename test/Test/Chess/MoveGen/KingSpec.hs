module Test.Chess.MoveGen.KingSpec where

import Chess.Base

import Chess.MoveGen
import Chess.MoveGen.King

import Test.Placements
import Test.Placements.King

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialKingMoves" $
         context "movement" $ do
           it "never produces moves off the board" $
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialKingMoves (placePiece emptyTest (Piece King White) c) c

           it "allows the king to move to any square in its Moore neighbourhood" $
             potentialKingMoves onlyKingTest { castlingRights = CastleRights False False False False } (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the white king to castle kingside, if he has the right to" $
             potentialKingMoves whiteKingOOTest { castlingRights = CastleRights True False False False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle kingside, if he does not have the right to" $
             potentialKingMoves whiteKingOOTest { castlingRights = CastleRights False False False False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the white king to castle queenside, if he has the right to" $
             potentialKingMoves whiteKingOOOTest { castlingRights = CastleRights False False True False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle queenside, if he does not have the right to" $
             potentialKingMoves whiteKingOOOTest { castlingRights = CastleRights False False False False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the white king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves whiteKingBothCastlesTest { castlingRights = CastleRights True False True False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle when he is not on his home square" $
             potentialKingMoves whiteKingMovedNoOOTest { castlingRights = CastleRights True True True True } (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle kingside when the rook is not on its home square" $
             potentialKingMoves whiteKingNoRookCastleTest { castlingRights = CastleRights True False False False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle queenside when the rook is not on its home square" $
             potentialKingMoves whiteKingNoRookCastleTest { castlingRights = CastleRights False False True False } (Coordinate 'e' 1) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle kingside, if he has the right to" $
             potentialKingMoves blackKingOOTest { castlingRights = CastleRights False True False False } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle kingside, if he does not have the right to" $
             potentialKingMoves blackKingOOTest { castlingRights = CastleRights False False False False } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle queenside, if he has the right to" $
             potentialKingMoves blackKingOOOTest { castlingRights = CastleRights False False False True } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle queenside, if he does not have the right to" $
             potentialKingMoves blackKingOOOTest { castlingRights = CastleRights False False False False } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves blackKingBothCastlesTest { castlingRights = CastleRights False True False True } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle when he is not on his home square" $
             potentialKingMoves blackKingMovedNoOOTest { castlingRights = CastleRights True True True True } (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle kingside when the rook is not on its home square" $
             potentialKingMoves blackKingNoRookCastleTest { castlingRights = CastleRights False True False False } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle queenside when the rook is not on its home square" $
             potentialKingMoves blackKingNoRookCastleTest { castlingRights = CastleRights False False False True } (Coordinate 'e' 8) `shouldBe`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the king to castle if the intermediate squares are occupied" $
             potentialKingMoves startingPos { castlingRights = CastleRights True False False False } (Coordinate 'e' 1) `shouldBe`
               []
