module Test.Chess.MoveGenSpec where

import Chess.Base

import Chess.MoveGen

import Control.Monad

import Test.Placements
import Test.Placements.Bishop
import Test.Placements.Rook

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = context "potential move generation" $ do
    describe "alongRay" $ do
      context "verticals" $ do
        it "returns a list of coordinates along the north vertical ray, including the destination" $
          alongRay (Coordinate 'a' 1, Coordinate 'a' 8) `shouldMatchList`
            [ Coordinate 'a' 2
            , Coordinate 'a' 3
            , Coordinate 'a' 4
            , Coordinate 'a' 5
            , Coordinate 'a' 6
            , Coordinate 'a' 7
            , Coordinate 'a' 8
            ]

        it "returns a list of coordinates along the south vertical ray, including the destination" $
          alongRay (Coordinate 'a' 8, Coordinate 'a' 1) `shouldMatchList`
            [ Coordinate 'a' 7
            , Coordinate 'a' 6
            , Coordinate 'a' 5
            , Coordinate 'a' 4
            , Coordinate 'a' 3
            , Coordinate 'a' 2
            , Coordinate 'a' 1
            ]

      context "horizontals" $ do
        it "returns a list of coordinates along the east horizontal ray, including the destination" $
          alongRay (Coordinate 'a' 1, Coordinate 'h' 1) `shouldMatchList`
            [ Coordinate 'b' 1
            , Coordinate 'c' 1
            , Coordinate 'd' 1
            , Coordinate 'e' 1
            , Coordinate 'f' 1
            , Coordinate 'g' 1
            , Coordinate 'h' 1
            ]

        it "returns a list of coordinates along the west horizontal ray, including the destination" $
          alongRay (Coordinate 'h' 1, Coordinate 'a' 1) `shouldMatchList`
            [ Coordinate 'g' 1
            , Coordinate 'f' 1
            , Coordinate 'e' 1
            , Coordinate 'd' 1
            , Coordinate 'c' 1
            , Coordinate 'b' 1
            , Coordinate 'a' 1
            ]

    describe "isBlocked" $ do

      context "verticals" $ do
        it "returns True if the destination point is behind another friendly piece along the ray" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 1, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns True if the destination point is behind another enemy piece along the ray" $
          isBlocked (placement rookCaptureVerticalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns False if the destination point is in front of another piece along the ray" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the ray is not blocked, moving north" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 8, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the ray is not blocked, moving south" $
          isBlocked (placement onlyRookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the destination square is occupied by an enemy piece" $
          isBlocked (placement rookCaptureVerticalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 7, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

      context "horizontals" $ do
        it "returns True if the destination point is behind another piece along the ray" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'h' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns True if the destination point is behind another enemy piece along the ray" $
          isBlocked (placement rookCaptureHorizontalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'g' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns False if the destination point is in front of another piece along the ray" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the ray is not blocked, moving west" $
          isBlocked (placement rookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'a' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the ray is not blocked, moving east" $
          isBlocked (placement onlyRookTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'h' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the destination square is occupied by an enemy piece" $
          isBlocked (placement rookCaptureHorizontalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'f' 5, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

      context "diagonals" $ do
        it "returns True if the destination point is behind another piece along the ray" $
          isBlocked (placement diagonalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'g' 8, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns True if the destination point is behind another piece along the ray" $
          isBlocked (placement bishopCaptureTest) Move { moveFrom = Coordinate 'a' 1, moveTo = Coordinate 'h' 8, moveType = Standard, movePromoteTo = Nothing } `shouldBe` True

        it "returns False if the destination point is in front of another piece along the ray" $
          isBlocked (placement diagonalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the ray is not blocked" $
          isBlocked (placement diagonalTest) Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'a' 8, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False

        it "returns False if the destination square is occupied by an enemy piece" $
          isBlocked (placement bishopCaptureTest) Move { moveFrom = Coordinate 'a' 1, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing } `shouldBe` False
