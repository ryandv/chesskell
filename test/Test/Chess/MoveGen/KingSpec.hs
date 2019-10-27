module Test.Chess.MoveGen.KingSpec where

import Chess.Base
import Chess.Bitboard

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
           -- TODO: add quickcheck spec for when CastleRights are True
           it "never produces moves off the board" $
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialKingMoves
               (CastleRights False False False False)
               (regularToBitboard . placement $ (placePiece emptyTest (Piece King White) c))
               White
               c

           it "allows the king to move to any square in its Moore neighbourhood" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ onlyKingTest) White (Coordinate 'd' 4) `shouldMatchList`
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
             potentialKingMoves (CastleRights True False False False) (regularToBitboard . placement $ whiteKingOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle kingside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ whiteKingOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the white king to castle queenside, if he has the right to" $
             potentialKingMoves (CastleRights False False True False) (regularToBitboard . placement $ whiteKingOOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle queenside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ whiteKingOOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the white king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves (CastleRights True False True False) (regularToBitboard . placement $ whiteKingBothCastlesTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'g' 1, moveType = Castle, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'c' 1, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle when he is not on his home square" $
             potentialKingMoves (CastleRights True True True True) (regularToBitboard . placement $ whiteKingMovedNoOOTest) White (Coordinate 'd' 4) `shouldMatchList`
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
             potentialKingMoves (CastleRights True False False False) (regularToBitboard . placement $ whiteKingNoRookCastleTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the white king to castle queenside when the rook is not on its home square" $
             potentialKingMoves (CastleRights False False True False) (regularToBitboard . placement $ whiteKingNoRookCastleTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle kingside, if he has the right to" $
             potentialKingMoves (CastleRights False True False False) (regularToBitboard . placement $ blackKingOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle kingside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ blackKingOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle queenside, if he has the right to" $
             potentialKingMoves (CastleRights False False False True) (regularToBitboard . placement $ blackKingOOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle queenside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ blackKingOOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "allows the black king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves (CastleRights False True False True) (regularToBitboard . placement $ blackKingBothCastlesTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'g' 8, moveType = Castle, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'c' 8, moveType = Castle, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle when he is not on his home square" $
             potentialKingMoves (CastleRights True True True True) (regularToBitboard . placement $ blackKingMovedNoOOTest) Black (Coordinate 'd' 4) `shouldMatchList`
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
             potentialKingMoves (CastleRights False True False False) (regularToBitboard . placement $ blackKingNoRookCastleTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the black king to castle queenside when the rook is not on its home square" $
             potentialKingMoves (CastleRights False False False True) (regularToBitboard . placement $ blackKingNoRookCastleTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'f' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 8, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "does not allow the king to castle if the intermediate squares are occupied" $
             potentialKingMoves (CastleRights True False False False) (regularToBitboard . placement $ startingPos) White (Coordinate 'e' 1) `shouldMatchList`
               []
