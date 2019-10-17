module Test.Chess.MoveGen.RookSpec where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen
import Chess.MoveGen.Rook

import Test.Placements
import Test.Placements.Rook

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialRookMoves" $ do
         it "never produces moves off the board" $ do
           let position = (\c -> (placePiece emptyTest (Piece Rook White) c))
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialRookMoves (placement $ position c) (totalOccupancyFor . placement $ position c) White c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialRookMoves (placement onlyRookTest) (totalOccupancyFor $ placement onlyRookTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'f' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'b' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'g' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'a' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'h' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "produces the correct set of moves when blocked by some pieces" $
           potentialRookMoves (placement rookTest) (totalOccupancyFor $ placement rookTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'b' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'a' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing }
             ]

         it "produces the correct set of moves, including captures" $
           potentialRookMoves (placement rookAllCapturesTest) (totalOccupancyFor $ placement rookAllCapturesTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'c' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'f' 5, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'b' 5, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 7, moveType = Capture, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 3, moveType = Capture, movePromoteTo = Nothing }
             ]

         it "does not allow a rook to capture pieces of its own color" $ do
           let position = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                         , (Piece Rook White, Coordinate 'h' 1)
                                         ])
           potentialRookMoves (placement position) (totalOccupancyFor $ placement position) White (Coordinate 'h' 1) `shouldMatchList`
             [ Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'g' 1, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 2, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'f' 1, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 3, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 4, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 5, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 6, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 7, moveType = Standard, movePromoteTo = Nothing }
             , Move { moveFrom = Coordinate 'h' 1, moveTo = Coordinate 'h' 8, moveType = Standard, movePromoteTo = Nothing }
             ]
