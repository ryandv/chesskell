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
             [ Move (Coordinate 'e' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 4)
             , Move (Coordinate 'e' 5) (Coordinate 'd' 4)
             , Move (Coordinate 'e' 5) (Coordinate 'c' 7)
             , Move (Coordinate 'e' 5) (Coordinate 'g' 7)
             , Move (Coordinate 'e' 5) (Coordinate 'g' 3)
             , Move (Coordinate 'e' 5) (Coordinate 'c' 3)
             , Move (Coordinate 'e' 5) (Coordinate 'b' 8)
             , Move (Coordinate 'e' 5) (Coordinate 'h' 8)
             , Move (Coordinate 'e' 5) (Coordinate 'h' 2)
             , Move (Coordinate 'e' 5) (Coordinate 'b' 2)
             , Move (Coordinate 'e' 5) (Coordinate 'a' 1)
             ]

         it "produces the correct set of moves when blocked by some pieces" $
           potentialBishopMoves (regularToBitboard . placement $ bishopTest) White (Coordinate 'e' 5) `shouldMatchList`
             [ Move (Coordinate 'e' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 4)
             , Move (Coordinate 'e' 5) (Coordinate 'd' 4)
             , Move (Coordinate 'e' 5) (Coordinate 'c' 7)
             , Move (Coordinate 'e' 5) (Coordinate 'g' 3)
             , Move (Coordinate 'e' 5) (Coordinate 'b' 8)
             , Move (Coordinate 'e' 5) (Coordinate 'h' 2)
             ]

         it "produces the correct set of moves, including captures" $
           potentialBishopMoves (regularToBitboard . placement $ bishopAllCapturesTest) White (Coordinate 'e' 5) `shouldMatchList`
             [ Move (Coordinate 'e' 5) (Coordinate 'd' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 6)
             , Move (Coordinate 'e' 5) (Coordinate 'f' 4)
             , Move (Coordinate 'e' 5) (Coordinate 'd' 4)
             , Capture (Coordinate 'e' 5) (Coordinate 'c' 7)
             , Capture (Coordinate 'e' 5) (Coordinate 'g' 7)
             , Capture (Coordinate 'e' 5) (Coordinate 'g' 3)
             , Capture (Coordinate 'e' 5) (Coordinate 'c' 3)
             ]
