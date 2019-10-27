module Chess.MoveGen
  ( pseudoLegalMoves
  ) where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Common
import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Rook

import Data.Maybe

import Control.Applicative

pseudoLegalMoves               :: RegularGame -> [Move]
pseudoLegalMoves game = (concatMap . concatMap) (pseudoLegalMovesFrom bitboard (castlingRights game) (enPassantSquare game)) $ placement game
  where bitboard = regularToBitboard $ placement game

pseudoLegalMovesFrom                                                                         :: BitboardRepresentation -> CastleRights -> Maybe Coordinate -> Square -> [Move]
pseudoLegalMovesFrom _ _ _ (Square Nothing _)                                                = []
pseudoLegalMovesFrom bitboard castlingRights enPassantSquare (Square (Just (Piece p ply)) l) | p == Pawn   = potentialPawnMoves enPassantSquare bitboard ply l
                                                                                             | p == Knight = potentialKnightMoves bitboard l
                                                                                             | p == Bishop = bishopMoves
                                                                                             | p == Rook   = rookMoves
                                                                                             | p == Queen  = bishopMoves ++ rookMoves
                                                                                             | p == King   = potentialKingMoves castlingRights bitboard l
  where bishopMoves = potentialBishopMoves bitboard ply l
        rookMoves   = potentialRookMoves bitboard ply l
        occupancy   = totalOccupancy bitboard
