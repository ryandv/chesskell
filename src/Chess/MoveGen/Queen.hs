module Chess.MoveGen.Queen where

import Chess.Base

import Chess.MoveGen.Bishop
import Chess.MoveGen.Rook

potentialQueenMoves                 :: RegularBoardRepresentation -> Player -> Coordinate -> [Move]
potentialQueenMoves placement ply c = potentialRookMoves placement ply c ++ potentialBishopMoves placement ply c
