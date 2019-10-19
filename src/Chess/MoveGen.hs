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
                               (Square (Just (Piece p ply)) l) | p == Pawn   = potentialPawnMoves enPassantSquare placement l
                                                               | p == Knight = potentialKnightMoves placement bitboard l
                                                               | p == Bishop = potentialBishopMoves bitboard (totalOccupancyFor placement) ply l
                                                               | p == Rook   = potentialRookMoves bitboard (totalOccupancyFor placement) ply l
                                                               | p == Queen  = potentialQueenMoves bitboard (totalOccupancyFor placement) ply l
                                                               | p == King   = potentialKingMoves castlingRights placement bitboard l
  where bitboard = regularToBitboard placement
