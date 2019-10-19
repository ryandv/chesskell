module Chess.MoveGen.Queen where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Bishop
import Chess.MoveGen.Rook

potentialQueenMoves                          :: BitboardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialQueenMoves bitboard occupancy ply c = potentialRookMoves bitboard occupancy ply c ++ potentialBishopMoves bitboard occupancy ply c
