module Chess.MoveGen.Queen where

import Chess.Base

import Chess.MoveGen.Bishop
import Chess.MoveGen.Rook

potentialQueenMoves             :: RegularBoardRepresentation -> Coordinate -> [Move]
potentialQueenMoves placement c = potentialRookMoves placement c ++ potentialBishopMoves placement c
