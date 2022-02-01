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
           property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialRookMoves (regularToBitboard . placement $ position c) White c

         it "produces the correct set of moves without being blocked by pieces" $
           potentialRookMoves (regularToBitboard . placement $ onlyRookTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move (Coordinate 'd' 5) (Coordinate 'e' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'c' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 4)
             , Move (Coordinate 'd' 5) (Coordinate 'f' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'b' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 7)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 3)
             , Move (Coordinate 'd' 5) (Coordinate 'g' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'a' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 8)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 2)
             , Move (Coordinate 'd' 5) (Coordinate 'h' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 1)
             ]

         it "produces the correct set of moves when blocked by some pieces" $
           potentialRookMoves (regularToBitboard . placement $ rookTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move (Coordinate 'd' 5) (Coordinate 'e' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'c' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 4)
             , Move (Coordinate 'd' 5) (Coordinate 'b' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 7)
             , Move (Coordinate 'd' 5) (Coordinate 'a' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 8)
             ]

         it "produces the correct set of moves, including captures" $
           potentialRookMoves (regularToBitboard . placement $ rookAllCapturesTest) White (Coordinate 'd' 5) `shouldMatchList`
             [ Move (Coordinate 'd' 5) (Coordinate 'e' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'c' 5)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'd' 5) (Coordinate 'd' 4)
             , Capture (Coordinate 'd' 5) (Coordinate 'f' 5)
             , Capture (Coordinate 'd' 5) (Coordinate 'b' 5)
             , Capture (Coordinate 'd' 5) (Coordinate 'd' 7)
             , Capture (Coordinate 'd' 5) (Coordinate 'd' 3)
             ]

         it "does not allow a rook to capture pieces of its own color" $ do
           let position = (setupGame [ (Piece King White, Coordinate 'e' 1)
                                         , (Piece Rook White, Coordinate 'h' 1)
                                         ])
           potentialRookMoves (regularToBitboard . placement $ position) White (Coordinate 'h' 1) `shouldMatchList`
             [ Move (Coordinate 'h' 1) (Coordinate 'g' 1)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 2)
             , Move (Coordinate 'h' 1) (Coordinate 'f' 1)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 3)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 4)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 5)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 6)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 7)
             , Move (Coordinate 'h' 1) (Coordinate 'h' 8)
             ]
