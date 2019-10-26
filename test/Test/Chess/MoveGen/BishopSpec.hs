module Test.Chess.MoveGen.BishopSpec where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen
import Chess.MoveGen.Bishop

import Test.Placements
import Test.Placements.Bishop

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialBishopMoves" $ do
         it "never produces moves off the board" $ do
           let position = (\c -> (placePiece emptyTest (Piece Bishop White) c))
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialBishopMoves (regularToBitboard . placement $ position c) White c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialBishopMoves (regularToBitboard . placement $ onlyBishopTest) White (Coordinate 'e' 5) `shouldMatchList`
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
           potentialBishopMoves (regularToBitboard . placement $ bishopTest) White (Coordinate 'e' 5) `shouldMatchList`
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
           potentialBishopMoves (regularToBitboard . placement $ bishopAllCapturesTest) White (Coordinate 'e' 5) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'f' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 7, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 7, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'g' 3, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'c' 3, moveType = Capture, movePromoteTo = Nothing }
             ]
