module Test.Chess.MoveGen.KnightSpec where

import Chess.Base

import Chess.MoveGen
import Chess.MoveGen.Knight

import Control.Monad

import Test.Placements
import Test.Placements.Knight

import Test.Hspec
import Test.QuickCheck

coords :: Gen Coordinate
coords = choose ('a', 'h') >>= (\x -> liftM (Coordinate x) (choose (1, 8)))

spec :: Spec
spec = describe "potentialKnightMoves" $ do
         it "never produces moves off the board" $
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialKnightMoves (placePiece emptyTest (Piece Knight White) c) c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialKnightMoves onlyKnightTest (Coordinate 'd' 4) `shouldBe`
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
           potentialKnightMoves knightTest (Coordinate 'd' 4) `shouldBe`
             [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 5, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "can jump over pieces" $
           potentialKnightMoves startingPos (Coordinate 'b' 1) `shouldBe`
             [ Move { moveFrom = Coordinate 'b' 1, moveTo = Coordinate 'a' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'b' 1, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
             ]
