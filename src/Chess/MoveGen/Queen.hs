module Chess.MoveGen.Queen where

import Chess.Base

import Chess.MoveGen.Bishop
import Chess.MoveGen.Rook

potentialQueenMoves     :: RegularGame -> Coordinate -> [Move]
potentialQueenMoves g c = potentialRookMoves g c ++ potentialBishopMoves g c
