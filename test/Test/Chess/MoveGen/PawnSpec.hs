module Test.Chess.MoveGen.PawnSpec where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen
import Chess.MoveGen.Pawn

import Test.Placements
import Test.Placements.Pawn

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialPawnMoves" $ do
         context "movement" $ do

           -- TODO: add quickcheck spec when enPassant isJust.
           it "never produces moves off the board" $ do
             let position c = placement (placePiece emptyTest (Piece Pawn White) c)
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialPawnMoves Nothing (regularToBitboard $ position c) White c

           it "allows double-jumping from the second rank for White" $ do
             potentialPawnMoves Nothing (regularToBitboard $ placement startingPos) White (Coordinate 'e' 2) `shouldMatchList`
               [ Move (Coordinate 'e' 2) (Coordinate 'e' 4)
               , Move (Coordinate 'e' 2) (Coordinate 'e' 3)
               ]

           it "disallows White double-jumping from elsewhere" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 6) ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'e' 6) `shouldMatchList`
                 [ Move (Coordinate 'e' 6) (Coordinate 'e' 7) ]

           it "allows double-jumping from the second rank for Black" $
             potentialPawnMoves Nothing  (regularToBitboard $ placement startingPos) Black (Coordinate 'e' 7) `shouldMatchList`
               [ Move (Coordinate 'e' 7) (Coordinate 'e' 5)
               , Move (Coordinate 'e' 7) (Coordinate 'e' 6)
               ]

           it "disallows Black double-jumping from elsewhere" $ do
             let position = placement (setupGame [ (Piece Pawn Black, Coordinate 'e' 3) ])
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'e' 3) `shouldMatchList`
                 [ Move (Coordinate 'e' 3) (Coordinate 'e' 2) ]

           it "does not allow White to advance onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'd' 4) `shouldMatchList` []

           it "does not allow White to double-jump onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 4)
                          , (Piece Pawn White, Coordinate 'd' 2)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'd' 2) `shouldMatchList`
                 [ Move (Coordinate 'd' 2) (Coordinate 'd' 3) ]

           it "does not allow Black to double-jump onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                          , (Piece Pawn White, Coordinate 'd' 5)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'd' 7) `shouldMatchList`
                 [ Move (Coordinate 'd' 7) (Coordinate 'd' 6) ]

           it "does not allow White to double-jump when the square in front is blocked" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 3)
                          , (Piece Pawn White, Coordinate 'd' 2)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'd' 2) `shouldMatchList` []

           it "does not allow Black to double-jump when the square in front is blocked" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                          , (Piece Pawn White, Coordinate 'd' 6)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'd' 7) `shouldMatchList` []


           it "does not allow Black to advance onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'd' 5) `shouldMatchList` []

         context "promotion" $ do
           it "produces four separate promotion moves when a White pawn moves to the eighth rank" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 7) ]))
             potentialPawnMoves Nothing (regularToBitboard position) White (Coordinate 'e' 7) `shouldMatchList`
               [ Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Rook White)
               , Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Knight White)
               , Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Bishop White)
               , Promote (Coordinate 'e' 7) (Coordinate 'e' 8) (Piece Queen White)
               ]

           it "produces four separate promotion moves when a Black pawn moves to the first rank" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'e' 2) ]))
             potentialPawnMoves Nothing (regularToBitboard position) Black (Coordinate 'e' 2) `shouldMatchList`
               [ Promote (Coordinate 'e' 2) (Coordinate 'e' 1) (Piece Rook Black)
               , Promote (Coordinate 'e' 2) (Coordinate 'e' 1) (Piece Knight Black)
               , Promote (Coordinate 'e' 2) (Coordinate 'e' 1) (Piece Bishop Black)
               , Promote (Coordinate 'e' 2) (Coordinate 'e' 1) (Piece Queen Black)
               ]

           it "does not allow White to capture the Black king when promoting" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 7)
                                                      , (Piece King Black, Coordinate 'e' 8)
                                                      ]))
             potentialPawnMoves Nothing
                                (regularToBitboard position)
                                White
                                (Coordinate 'e' 7) `shouldMatchList` []

         context "capturing" $ do

           it "allows White to capture pieces on the neighbouring NW and NE squares" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'c' 5)
                          , (Piece Pawn Black, Coordinate 'e' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'd' 4) `shouldMatchList`
                 [ Move (Coordinate 'd' 4) (Coordinate 'd' 5)
                 , Capture (Coordinate 'd' 4) (Coordinate 'c' 5)
                 , Capture (Coordinate 'd' 4) (Coordinate 'e' 5)
                 ]

           it "allows Black to capture pieces on the neighbouring SW and SE squares" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'c' 4)
                          , (Piece Pawn White, Coordinate 'e' 4)
                          , (Piece Pawn Black, Coordinate 'd' 5)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'd' 5) `shouldMatchList`
                 [ Move (Coordinate 'd' 5) (Coordinate 'd' 4)
                 , Capture (Coordinate 'd' 5) (Coordinate 'c' 4)
                 , Capture (Coordinate 'd' 5) (Coordinate 'e' 4)
                 ]

           it "does not allow White to capture its own pieces" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'c' 5)
                          , (Piece Pawn White, Coordinate 'e' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'd' 4) `shouldMatchList`
                 [ Move (Coordinate 'd' 4) (Coordinate 'd' 5)
                 ]

           it "does not allow Black to capture its own pieces" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'c' 4)
                           , (Piece Pawn Black, Coordinate 'e' 4)
                           , (Piece Pawn Black, Coordinate 'd' 5)
                           ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               Black
               (Coordinate 'd' 5) `shouldMatchList`
                 [ Move (Coordinate 'd' 5) (Coordinate 'd' 4)
                 ]

           it "only allows capturing on the 'b' file for pawns on the 'a' file" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'b' 5)
                          , (Piece Pawn White, Coordinate 'a' 4)
                          ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'a' 4) `shouldMatchList`
                 [ Move (Coordinate 'a' 4) (Coordinate 'a' 5)
                 , Capture (Coordinate 'a' 4) (Coordinate 'b' 5)
                 ]

           it "only allows capturing on the 'g' file for pawns on the 'h' file" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'g' 5)
                         , (Piece Pawn White, Coordinate 'h' 4)
                         ]))
             potentialPawnMoves
               Nothing
               (regularToBitboard position)
               White
               (Coordinate 'h' 4) `shouldMatchList`
                 [ Move (Coordinate 'h' 4) (Coordinate 'h' 5)
                 , Capture (Coordinate 'h' 4) (Coordinate 'g' 5)
                 ]

           it "allows White to en passant, if available" $ do
             let position = (placement whiteEnPassantTest)
             potentialPawnMoves (Just (Coordinate 'd' 6)) (regularToBitboard position) White (Coordinate 'e' 5) `shouldMatchList`
               [ Move (Coordinate 'e' 5) (Coordinate 'e' 6)
               , EnPassant (Coordinate 'e' 5) (Coordinate 'd' 6)
               ]

           it "allows Black to en passant, if available" $ do
             let position = (placement blackEnPassantTest)
             potentialPawnMoves (Just (Coordinate 'e' 3)) (regularToBitboard position) Black (Coordinate 'd' 4) `shouldMatchList`
               [ Move (Coordinate 'd' 4) (Coordinate 'd' 3)
               , EnPassant (Coordinate 'd' 4) (Coordinate 'e' 3)
               ]

           it "only allows en passant from the correct square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                                     , (Piece Pawn White, Coordinate 'e' 5)
                                     , (Piece Pawn White, Coordinate 'b' 2)
                                     ]))
             potentialPawnMoves
               (Just (Coordinate 'd' 6))
               (regularToBitboard position)
               White
               (Coordinate 'b' 2) `shouldMatchList`
                 [ Move (Coordinate 'b' 2) (Coordinate 'b' 4)
                 , Move (Coordinate 'b' 2) (Coordinate 'b' 3)
                 ]
