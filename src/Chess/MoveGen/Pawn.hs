module Chess.MoveGen.Pawn
  ( potentialPawnMoves
  ) where

import Chess.Base
import Chess.Board
import Chess.Bitboard

import Chess.MoveGen.Common

import Data.Maybe

potentialPawnMoves                                                  :: Maybe Coordinate -> BitboardRepresentation -> Player -> Coordinate -> [Move]
potentialPawnMoves Nothing bitboard ply c                           = standardPawnMoves bitboard ply c
potentialPawnMoves (Just enPassant) bitboard ply c@(Coordinate r f) = standardPawnMoves bitboard ply c ++ enPassantMoves enPassant
  where rankOffset :: Player -> Int
        rankOffset White = 1
        rankOffset Black = -1

        enPassantMoves                      :: Coordinate -> [Move]
        enPassantMoves (Coordinate r' f')   | canEnPassantFromThisRank r' && canEnPassantFromThisFile f' = [Move { moveFrom = (Coordinate r f)
                                                                                                           , moveTo = (Coordinate r' f')
                                                                                                           , moveType = EnPassant
                                                                                                           , movePromoteTo = Nothing }]
                                            | otherwise                                   = []

        canEnPassantFromThisRank r' = (toEnum $ fromEnum r' + (fromEnum $ rankOffset ply)) == r
        canEnPassantFromThisFile f' = (f == (f' - 1) || f == (f' + 1))

standardPawnMoves                                 :: BitboardRepresentation -> Player -> Coordinate -> [Move]
standardPawnMoves bitboard ply c@(Coordinate _ r) | r == 2 && (ply == White)  = whiteDoubleJump bitboard c ++ whiteAdvance bitboard c ++ whiteCaptures bitboard c
                                                  | r == 7 && (ply == Black)  = blackDoubleJump bitboard c ++ blackAdvance bitboard c ++ blackCaptures bitboard c
                                                  | (ply == White) = whiteAdvance bitboard c ++ whiteCaptures bitboard c
                                                  | (ply == Black) = blackAdvance bitboard c ++ blackCaptures bitboard c

  where whiteAdvance                       :: BitboardRepresentation -> Coordinate -> [Move]
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

whiteCaptures                             :: BitboardRepresentation -> Coordinate -> [Move]
whiteCaptures bitboard c@(Coordinate f _) | f == 'a' = whiteNECapture bitboard c
                                          | f == 'h' = whiteNWCapture bitboard c
                                          | otherwise = whiteNWCapture bitboard c ++ whiteNECapture bitboard c

blackCaptures                             :: BitboardRepresentation -> Coordinate -> [Move]
blackCaptures bitboard c@(Coordinate f _) | f == 'a' = blackSECapture bitboard c
                                          | f == 'h' = blackSWCapture bitboard c
                                          | otherwise = blackSWCapture bitboard c ++ blackSECapture bitboard c

capture                                       :: BitboardRepresentation -> Coordinate -> File -> Rank -> Player -> [Move]
capture bitboard (Coordinate f r) tf dr enemy | (r+dr) > 8 || (r+dr) < 1 = []
                                              | (isJust $ target) && fmap pieceOwner target == Just enemy = [Move { moveFrom = (Coordinate f r)
                                                                                                           , moveTo = targetCoord
                                                                                                           , moveType = Capture
                                                                                                           , movePromoteTo = Nothing }]
                                              | otherwise = [] where
  target = bitboardPieceAt bitboard (Coordinate tf (r+dr))
  targetCoord = Coordinate tf (r+dr)

whiteNWCapture                      :: BitboardRepresentation -> Coordinate -> [Move]
whiteNWCapture b c@(Coordinate f _) = capture b c (pred f) 1 Black

blackSWCapture                      :: BitboardRepresentation -> Coordinate -> [Move]
blackSWCapture b c@(Coordinate f _) = capture b c (pred f) (-1) White

whiteNECapture                      :: BitboardRepresentation -> Coordinate -> [Move]
whiteNECapture b c@(Coordinate f _) = capture b c (succ f) 1 Black

blackSECapture                      :: BitboardRepresentation -> Coordinate -> [Move]
blackSECapture b c@(Coordinate f _) = capture b c (succ f) (-1) White
