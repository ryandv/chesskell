module Test.Chess.MoveGen.QueenSpec where

import Chess.Base

import Chess.MoveGen
import Chess.MoveGen.Queen

import Test.Placements
import Test.Placements.Queen

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialQueenMoves" $
         context "movement" $ do

           it "never produces moves off the board" $
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialQueenMoves (placePiece emptyTest (Piece Queen White) c) c

           it "produces the correct set of moves without being blocked by pieces" $
             potentialQueenMoves onlyQueenTest (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'h' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'h' 8, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "produces the correct set of moves when blocked by some pieces" $
             potentialQueenMoves queenTest (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'h' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "produces the correct set of moves, including captures" $
             potentialQueenMoves queenCaptureTest (Coordinate 'd' 4) `shouldBe`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 7, moveType = Capture, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'h' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'f' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'b' 2, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 7, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 7, moveType = Capture, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'g' 1, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'a' 1, moveType = Standard, movePromoteTo = Nothing }
               ]

