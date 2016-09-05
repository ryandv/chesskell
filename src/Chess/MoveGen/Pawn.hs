module Chess.MoveGen.Pawn
  ( potentialPawnMoves
  ) where

import Chess.Base

import Chess.MoveGen.Common

import Data.Maybe

potentialPawnMoves                                               :: Maybe Coordinate -> RegularBoardRepresentation -> Coordinate -> [Move]
potentialPawnMoves Nothing placement c                           = standardPawnMoves placement c
potentialPawnMoves (Just enPassant) placement c@(Coordinate r f) = standardPawnMoves placement c ++ enPassantMoves enPassant where
  rankOffset :: Int
  rankOffset = case (fmap pieceOwner $ pieceAt placement c) of
                 Just White -> 1
                 Just Black -> -1

  enPassantMoves                      :: Coordinate -> [Move]
  enPassantMoves (Coordinate r' f')   | (toEnum $ fromEnum r' + fromEnum rankOffset) == r
                                        && (f == (f' - 1) || f == (f' + 1))         = [Move { moveFrom = (Coordinate r f)
                                                                                            , moveTo = (Coordinate r' f')
                                                                                            , moveType = EnPassant
                                                                                            , movePromoteTo = Nothing }]
                                      | otherwise                                   = []

standardPawnMoves                      :: RegularBoardRepresentation -> Coordinate -> [Move]
standardPawnMoves b c@(Coordinate _ r) | r == 2 && ((Just White) == pawnOwner)  = whiteDoubleJump b c ++ whiteAdvance b c ++ whiteCaptures b c
                                       | r == 7 && ((Just Black) == pawnOwner)  = blackDoubleJump b c ++ blackAdvance b c ++ blackCaptures b c
                                       | ((Just White) == pawnOwner) = whiteAdvance b c ++ whiteCaptures b c
                                       | ((Just Black) == pawnOwner) = blackAdvance b c ++ blackCaptures b c where
  pawnOwner :: Maybe Player
  pawnOwner = (fmap pieceOwner $ pieceAt b c)

whiteAdvance                       :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteAdvance b c                   = advance b c 1

blackAdvance                       :: RegularBoardRepresentation -> Coordinate -> [Move]
blackAdvance b c                   = advance b c (-1)

whiteDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteDoubleJump b c@(Coordinate f r) | not $ unoccupied b (Coordinate f (r+1)) = []
                                     | otherwise = advance b c 2

blackDoubleJump                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackDoubleJump b c@(Coordinate f r) | not $ unoccupied b (Coordinate f (r-1)) = []
                                     | otherwise = advance b c (-2)

advance                             :: RegularBoardRepresentation -> Coordinate -> Rank -> [Move]
advance b c@(Coordinate f r) offset | (r+offset) > 8 || (r+offset) < 1 = []
                                    | (unoccupied b $ Coordinate f (r+offset)) && (r+offset) == 8 && ((pieceAt b c) == (Just $ Piece Pawn White)) = map (Move (Coordinate f r) (Coordinate f (r+offset)) Promotion)
                                      [ Just $ Piece Rook White
                                      , Just $ Piece Knight White
                                      , Just $ Piece Bishop White
                                      , Just $ Piece Queen White ]
                                    | (unoccupied b $ Coordinate f (r+offset)) && (r+offset) == 1 && ((pieceAt b c) == (Just $ Piece Pawn Black)) = map (Move (Coordinate f r) (Coordinate f (r+offset)) Promotion)
                                      [ Just $ Piece Rook Black
                                      , Just $ Piece Knight Black
                                      , Just $ Piece Bishop Black
                                      , Just $ Piece Queen Black ]
                                    | (unoccupied b $ Coordinate f (r+offset)) = [Move { moveFrom = (Coordinate f r)
                                                                                     , moveTo = (Coordinate f (r+offset))
                                                                                     , moveType = Standard
                                                                                     , movePromoteTo = Nothing}]
                                  | otherwise                                = []

whiteCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteCaptures b c@(Coordinate f _)      | f == 'a' = whiteNECapture b c
                                        | f == 'h' = whiteNWCapture b c
                                        | otherwise = whiteNWCapture b c ++ whiteNECapture b c

blackCaptures                           :: RegularBoardRepresentation -> Coordinate -> [Move]
blackCaptures b c@(Coordinate f _)      | f == 'a' = blackNECapture b c
                                        | f == 'h' = blackNWCapture b c
                                        | otherwise = blackNWCapture b c ++ blackNECapture b c

capture                                :: RegularBoardRepresentation -> Coordinate -> File -> Rank -> Player -> [Move]
capture b (Coordinate f r) tf dr enemy | (r+dr) > 8 || (r+dr) < 1 = []
                                       | (isJust $ target) && fmap pieceOwner target == Just enemy = [Move { moveFrom = (Coordinate f r)
                                                                                                           , moveTo = targetCoord
                                                                                                           , moveType = Capture
                                                                                                           , movePromoteTo = Nothing }]
                                       | otherwise = [] where
  target = pieceAt b (Coordinate tf (r+dr))
  targetCoord = Coordinate tf (r+dr)

whiteNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteNWCapture b c@(Coordinate f _) = capture b c (pred f) 1 Black

blackNWCapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackNWCapture b c@(Coordinate f _) = capture b c (pred f) (-1) White

whiteNECapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
whiteNECapture b c@(Coordinate f _) = capture b c (succ f) 1 Black

blackNECapture                      :: RegularBoardRepresentation -> Coordinate -> [Move]
blackNECapture b c@(Coordinate f _) = capture b c (succ f) (-1) White
