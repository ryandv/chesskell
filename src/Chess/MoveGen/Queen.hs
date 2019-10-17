module Chess.MoveGen.Queen where

import Chess.Base
import Chess.Bitboard

import Chess.MoveGen.Bishop
import Chess.MoveGen.Rook

potentialQueenMoves                 :: RegularBoardRepresentation -> Bitboard -> Player -> Coordinate -> [Move]
potentialQueenMoves placement occupancy ply c = potentialRookMoves placement occupancy ply c ++ potentialBishopMoves placement occupancy ply c
