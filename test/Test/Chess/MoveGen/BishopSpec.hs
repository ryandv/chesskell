module Test.Chess.MoveGen.BishopSpec where

import Chess.Base

import Chess.MoveGen
import Chess.MoveGen.Bishop

import Test.Placements
import Test.Placements.Bishop

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialBishopMoves" $ do
         it "never produces moves off the board" $
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialBishopMoves (placement (placePiece emptyTest (Piece Bishop White) c)) c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialBishopMoves (placement onlyBishopTest) (Coordinate 'e' 5) `shouldBe`
             [ Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'b' 8, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'h' 8, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'h' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'b' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'a' 1, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "produces the correct set of moves when blocked by some pieces" $
           potentialBishopMoves (placement bishopTest) (Coordinate 'e' 5) `shouldBe`
             [ Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'b' 8, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'h' 2, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "produces the correct set of moves, including captures" $
           potentialBishopMoves (placement bishopAllCapturesTest) (Coordinate 'e' 5) `shouldBe`
             [ Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 7, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 7, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 3, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 3, moveType = Capture, movePromoteTo = Nothing }
             ]
