module Test.Chess.MoveGen.KnightSpec where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen
import Chess.MoveGen.Knight

import Test.Placements
import Test.Placements.Knight

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialKnightMoves" $ do
         it "never produces moves off the board" $ do
           let position c = (placement $ (placePiece emptyTest (Piece Knight White) c))
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialKnightMoves (regularToBitboard $ position c) White c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialKnightMoves (regularToBitboard . placement $ onlyKnightTest) White (Coordinate 'd' 4) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 5, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "produces the correct set of moves when blocked by some pieces" $
           potentialKnightMoves (regularToBitboard . placement $ knightTest) White (Coordinate 'd' 4) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 5, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "can jump over pieces" $
           potentialKnightMoves (regularToBitboard . placement $ startingPos) White (Coordinate 'b' 1) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'b' 1, moveTo = Coordinate 'a' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'b' 1, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
             ]
