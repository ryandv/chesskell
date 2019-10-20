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
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialPawnMoves Nothing (position c) (regularToBitboard $ position c) c

           it "allows double-jumping from the second rank for White" $ do
             potentialPawnMoves Nothing (placement startingPos) (regularToBitboard $ placement startingPos) (Coordinate 'e' 2) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 4, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 3, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "disallows White double-jumping from elsewhere" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 6) ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'e' 6) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'e' 6, moveTo = Coordinate 'e' 7, moveType = Standard, movePromoteTo = Nothing } ]

           it "allows double-jumping from the second rank for Black" $
             potentialPawnMoves Nothing (placement startingPos) (regularToBitboard $ placement startingPos) (Coordinate 'e' 7) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 5, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing }
               ]

           it "disallows Black double-jumping from elsewhere" $ do
             let position = placement (setupGame [ (Piece Pawn Black, Coordinate 'e' 3) ])
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'e' 3) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'e' 3, moveTo = Coordinate 'e' 2, moveType = Standard, movePromoteTo = Nothing } ]

           it "does not allow White to advance onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 4) `shouldMatchList` []

           it "does not allow White to double-jump onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 4)
                          , (Piece Pawn White, Coordinate 'd' 2)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 2) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 2, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing } ]

           it "does not allow Black to double-jump onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                          , (Piece Pawn White, Coordinate 'd' 5)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 7) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 7, moveTo = Coordinate 'd' 6, moveType = Standard, movePromoteTo = Nothing } ]

           it "does not allow White to double-jump when the square in front is blocked" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 3)
                          , (Piece Pawn White, Coordinate 'd' 2)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 2) `shouldMatchList` []

           it "does not allow Black to double-jump when the square in front is blocked" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                          , (Piece Pawn White, Coordinate 'd' 6)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 7) `shouldMatchList` []


           it "does not allow Black to advance onto an occupied square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 5) `shouldMatchList` []

         context "promotion" $ do
           it "produces four separate promotion moves when a White pawn moves to the eighth rank" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 7) ]))
             potentialPawnMoves Nothing position (regularToBitboard position) (Coordinate 'e' 7) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion, movePromoteTo = Just $ Piece Rook White }
               , Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion, movePromoteTo = Just $ Piece Knight White }
               , Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion, movePromoteTo = Just $ Piece Bishop White }
               , Move { moveFrom = Coordinate 'e' 7, moveTo = Coordinate 'e' 8, moveType = Promotion, movePromoteTo = Just $ Piece Queen White }
               ]

           it "produces four separate promotion moves when a Black pawn moves to the first rank" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'e' 2) ]))
             potentialPawnMoves Nothing position (regularToBitboard position) (Coordinate 'e' 2) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 1, moveType = Promotion, movePromoteTo = Just $ Piece Rook Black }
               , Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 1, moveType = Promotion, movePromoteTo = Just $ Piece Knight Black }
               , Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 1, moveType = Promotion, movePromoteTo = Just $ Piece Bishop Black }
               , Move { moveFrom = Coordinate 'e' 2, moveTo = Coordinate 'e' 1, moveType = Promotion, movePromoteTo = Just $ Piece Queen Black }
               ]

           it "does not allow White to capture the Black king when promoting" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'e' 7)
                                                      , (Piece King Black, Coordinate 'e' 8)
                                                      ]))
             potentialPawnMoves Nothing
                                position
                                (regularToBitboard position)
                                (Coordinate 'e' 7) `shouldMatchList` []

         context "capturing" $ do

           it "allows White to capture pieces on the neighbouring NW and NE squares" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'c' 5)
                          , (Piece Pawn Black, Coordinate 'e' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 4) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'c' 5, moveType = Capture, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 5, moveType = Capture, movePromoteTo = Nothing }
                 ]

           it "allows Black to capture pieces on the neighbouring NW and NE squares" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'c' 4)
                          , (Piece Pawn White, Coordinate 'e' 4)
                          , (Piece Pawn Black, Coordinate 'd' 5)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 5) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'c' 4, moveType = Capture, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'e' 4, moveType = Capture, movePromoteTo = Nothing }
                 ]

           it "does not allow White to capture its own pieces" $ do
             let position = (placement (setupGame [ (Piece Pawn White, Coordinate 'c' 5)
                          , (Piece Pawn White, Coordinate 'e' 5)
                          , (Piece Pawn White, Coordinate 'd' 4)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 4) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 5, moveType = Standard, movePromoteTo = Nothing }
                 ]

           it "does not allow Black to capture its own pieces" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'c' 4)
                           , (Piece Pawn Black, Coordinate 'e' 4)
                           , (Piece Pawn Black, Coordinate 'd' 5)
                           ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'd' 5) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'd' 5, moveTo = Coordinate 'd' 4, moveType = Standard, movePromoteTo = Nothing }
                 ]

           it "only allows capturing on the 'b' file for pawns on the 'a' file" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'b' 5)
                          , (Piece Pawn White, Coordinate 'a' 4)
                          ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'a' 4) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'a' 4, moveTo = Coordinate 'a' 5, moveType = Standard, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'a' 4, moveTo = Coordinate 'b' 5, moveType = Capture, movePromoteTo = Nothing }
                 ]

           it "only allows capturing on the 'g' file for pawns on the 'h' file" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'g' 5)
                         , (Piece Pawn White, Coordinate 'h' 4)
                         ]))
             potentialPawnMoves
               Nothing
               position
               (regularToBitboard position)
               (Coordinate 'h' 4) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'h' 4, moveTo = Coordinate 'h' 5, moveType = Standard, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'h' 4, moveTo = Coordinate 'g' 5, moveType = Capture, movePromoteTo = Nothing }
                 ]

           it "allows White to en passant, if available" $ do
             let position = (placement whiteEnPassantTest)
             potentialPawnMoves (Just (Coordinate 'd' 6)) position (regularToBitboard position) (Coordinate 'e' 5) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'e' 6, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'e' 5, moveTo = Coordinate 'd' 6, moveType = EnPassant, movePromoteTo = Nothing }
               ]

           it "allows Black to en passant, if available" $ do
             let position = (placement blackEnPassantTest)
             potentialPawnMoves (Just (Coordinate 'e' 3)) position (regularToBitboard position) (Coordinate 'd' 4) `shouldMatchList`
               [ Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'd' 3, moveType = Standard, movePromoteTo = Nothing }
               , Move { moveFrom = Coordinate 'd' 4, moveTo = Coordinate 'e' 3, moveType = EnPassant, movePromoteTo = Nothing }
               ]

           it "only allows en passant from the correct square" $ do
             let position = (placement (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                                     , (Piece Pawn White, Coordinate 'e' 5)
                                     , (Piece Pawn White, Coordinate 'b' 2)
                                     ]))
             potentialPawnMoves
               (Just (Coordinate 'd' 6))
               position
               (regularToBitboard position)
               (Coordinate 'b' 2) `shouldMatchList`
                 [ Move { moveFrom = Coordinate 'b' 2, moveTo = Coordinate 'b' 4, moveType = Standard, movePromoteTo = Nothing }
                 , Move { moveFrom = Coordinate 'b' 2, moveTo = Coordinate 'b' 3, moveType = Standard, movePromoteTo = Nothing }
                 ]
