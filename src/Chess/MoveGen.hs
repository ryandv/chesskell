module Chess.MoveGen
  ( alongRay
  , isBlocked
  , pseudoLegalMoves
  ) where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common
import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Queen
import Chess.MoveGen.Rook

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: RegularGame -> [Move]
pseudoLegalMoves game@Game { placement = b } = (concatMap . concatMap) (pseudoLegalMovesFrom game) b

pseudoLegalMovesFrom :: RegularGame -> Square -> [Move]
pseudoLegalMovesFrom _ (Square Nothing _)            = []
pseudoLegalMovesFrom game@Game { placement = placement
                               , castlingRights = castlingRights
                               , enPassantSquare = enPassantSquare
                               }
                               (Square (Just (Piece p ply)) l) | p == Pawn   = potentialPawnMoves enPassantSquare placement bitboard l
                                                               | p == Knight = potentialKnightMoves placement bitboard l
                                                               | p == Bishop = bishopMoves
                                                               | p == Rook   = rookMoves
                                                               | p == Queen  = bishopMoves ++ rookMoves
                                                               | p == King   = potentialKingMoves castlingRights placement bitboard l
  where bitboard = regularToBitboard placement
        bishopMoves = potentialBishopMoves bitboard occupancy ply l
        rookMoves   = potentialRookMoves bitboard occupancy ply l
        occupancy = totalOccupancyFor placement
        diagonals = [NW, NE, SW, SE]
        straights = [N, E, S, W]
