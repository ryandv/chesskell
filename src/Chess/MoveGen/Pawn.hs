module Chess.MoveGen.Pawn
  ( potentialPawnMoves
  ) where

import Chess.Base
import Chess.Board
import Chess.Bitboard

import Chess.MoveGen.Common

import Data.Maybe

potentialPawnMoves                                                        :: Maybe Coordinate -> RegularBoardRepresentation -> BitboardRepresentation -> Coordinate -> [Move]
potentialPawnMoves Nothing placement bitboard c                           = standardPawnMoves placement bitboard c
potentialPawnMoves (Just enPassant) placement bitboard c@(Coordinate r f) = standardPawnMoves placement bitboard c ++ enPassantMoves enPassant where
  rankOffset :: Int
  rankOffset = case (fmap pieceOwner $ bitboardPieceAt bitboard c) of
                 Just White -> 1
                 Just Black -> -1

  enPassantMoves                      :: Coordinate -> [Move]
  enPassantMoves (Coordinate r' f')   | (toEnum $ fromEnum r' + fromEnum rankOffset) == r
                                        && (f == (f' - 1) || f == (f' + 1))         = [Move { moveFrom = (Coordinate r f)
                                                                                            , moveTo = (Coordinate r' f')
                                                                                            , moveType = EnPassant
                                                                                            , movePromoteTo = Nothing }]
                                      | otherwise                                   = []

standardPawnMoves                      :: RegularBoardRepresentation -> BitboardRepresentation -> Coordinate -> [Move]
standardPawnMoves b bitboard c@(Coordinate _ r) | r == 2 && ((Just White) == pawnOwner)  = whiteDoubleJump bitboard c ++ whiteAdvance bitboard c ++ whiteCaptures b c
                                       | r == 7 && ((Just Black) == pawnOwner)  = blackDoubleJump bitboard c ++ blackAdvance bitboard c ++ blackCaptures b c
                                       | ((Just White) == pawnOwner) = whiteAdvance bitboard c ++ whiteCaptures b c
                                       | ((Just Black) == pawnOwner) = blackAdvance bitboard c ++ blackCaptures b c where
  pawnOwner :: Maybe Player
  pawnOwner = (fmap pieceOwner $ pieceAt b c)

whiteAdvance                       :: BitboardRepresentation -> Coordinate -> [Move]
whiteAdvance b c                   = advance b c 1

blackAdvance                       :: BitboardRepresentation -> Coordinate -> [Move]
blackAdvance b c                   = advance b c (-1)

whiteDoubleJump                      :: BitboardRepresentation -> Coordinate -> [Move]
whiteDoubleJump b c@(Coordinate f r) | bitboardIsOccupied b (Coordinate f (r+1)) = []
                                     | otherwise = advance b c 2

blackDoubleJump                      :: BitboardRepresentation -> Coordinate -> [Move]
blackDoubleJump b c@(Coordinate f r) | bitboardIsOccupied b (Coordinate f (r-1)) = []
                                     | otherwise = advance b c (-2)

advance                             :: BitboardRepresentation -> Coordinate -> Rank -> [Move]
advance b c@(Coordinate f r) offset | destinationRank > 8 || destinationRank < 1 = []
                                    | whiteCanPromote && movingPieceIsWhite = map (Move (Coordinate f r) destinationCoordinate Promotion)
                                      [ Just $ Piece Rook White
                                      , Just $ Piece Knight White
                                      , Just $ Piece Bishop White
                                      , Just $ Piece Queen White
                                      ]
                                    | blackCanPromote && movingPieceIsBlack = map (Move (Coordinate f r) destinationCoordinate Promotion)
                                      [ Just $ Piece Rook Black
                                      , Just $ Piece Knight Black
                                      , Just $ Piece Bishop Black
                                      , Just $ Piece Queen Black
                                      ]
                                    | destinationUnoccupied = [Move { moveFrom      = (Coordinate f r)
                                                                    , moveTo        = destinationCoordinate
                                                                    , moveType      = Standard
                                                                    , movePromoteTo = Nothing}]
                                    | otherwise             = []

  where destinationRank = r + offset
        destinationCoordinate = Coordinate f (r + offset)
        movingPieceIsWhite = movingPiece == (Just $ Piece Pawn White)
        whiteCanPromote = (not $ bitboardIsOccupied b destinationCoordinate) && destinationRank == 8
        movingPieceIsBlack = movingPiece == (Just $ Piece Pawn Black)
        blackCanPromote = (not $ bitboardIsOccupied b destinationCoordinate) && destinationRank == 1
        destinationUnoccupied = (not . bitboardIsOccupied b $ Coordinate f (r+offset))
        movingPiece = bitboardPieceAt b c

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
