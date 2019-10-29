module Chess.Predicates
  ( isAttacked
  , isChecked
  , isCheckmate
  , isCastleSafe
  , isStalemate
  , moveIsLegal
  ) where

import Chess.Base
import Chess.Board
import Chess.Bitboard
import Chess.MoveGen

import Control.Applicative

import Data.Maybe

-- Given an opponent's PieceType, this function works by placing down a ghost piece of the same PieceType but the active player's color,
-- and checks to see if the ghost piece can capture any of the opponent's pieces.
isAttacked :: Game BitboardRepresentation -> Coordinate -> Bool
isAttacked game coord = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where
  bitboards = (placement game)

  rookAttacks = (potentialRookMoves bitboards activePly coord)
  bishopAttacks = (potentialBishopMoves bitboards activePly coord)

  activePly = (activeColor game)

  isQueenChecking :: Bool
  isQueenChecking = fastIsChecking (rookAttacks ++ bishopAttacks) bitboards coord Queen

  isRookChecking :: Bool
  isRookChecking = fastIsChecking rookAttacks bitboards coord Rook

  isBishopChecking :: Bool
  isBishopChecking = fastIsChecking bishopAttacks bitboards coord Bishop

  isKnightChecking :: Bool
  isKnightChecking = fastIsChecking (potentialKnightMoves bitboards activePly coord) bitboards coord Knight

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = fastIsChecking (potentialPawnMoves (enPassantSquare game) bitboards activePly coord) bitboards coord Pawn

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = fastIsChecking (potentialKingMoves (castlingRights game) bitboards activePly coord) bitboards coord King

isChecked      :: Game BitboardRepresentation -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare White = indicesToCoordinate . squareIndexToIndices $ bitscanForward (whiteKings $ placement game)
  kingSquare Black = indicesToCoordinate . squareIndexToIndices $ bitscanForward (blackKings $ placement game)

isCheckmate          :: Game BitboardRepresentation -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate          :: Game BitboardRepresentation -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

moveIsLegal :: Move -> Game BitboardRepresentation -> Bool
moveIsLegal move@Move{ moveFrom = from } game = moveIsPseudoLegal && moveIsByRightPlayer && (notCheckedAfterMove game move) where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (bitboardPieceAt position from)) == (Just (activeColor game))
  position = placement game

notCheckedAfterMove :: Game BitboardRepresentation -> Move -> Bool
notCheckedAfterMove game move = (not $ isChecked game { placement = bitboardMovePiece (placement game) move })

noLegalMovesRemaining :: Game BitboardRepresentation -> Player -> Bool
noLegalMovesRemaining game ply = null
                               $ filter (liftA2 (&&) pieceIsOwnedByPly (notCheckedAfterMove game))
                               $ pseudoLegalMoves game where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (bitboardPieceAt (placement game) from)) == (Just ply)

fastIsChecking :: [Move] -> BitboardRepresentation -> Coordinate -> PieceType -> Bool
fastIsChecking moves bitboard coord pt = not
                                       . null
                                       . filter (liftA2 (&&) moveIsCapture (moveMatchesPieceType pt))
                                       . filter moveIsFromTargetCoordinate
                                       $ moves

  where moveIsCapture :: Move -> Bool
        moveIsCapture = (== Capture) . moveType

        moveMatchesPieceType    :: PieceType -> Move -> Bool
        moveMatchesPieceType pt = (== (Just pt)) . (fmap pieceType) . bitboardPieceAt bitboard . moveTo

        moveIsFromTargetCoordinate :: Move -> Bool
        moveIsFromTargetCoordinate = (== coord) . moveFrom

isCastleSafe :: CastleSide -> Game BitboardRepresentation -> Player -> Bool
isCastleSafe Kingside game White = all (not . isAttacked game) [Coordinate 'f' 1, Coordinate 'g' 1]
isCastleSafe Kingside game Black = all (not . isAttacked game) [Coordinate 'f' 8, Coordinate 'g' 8]
isCastleSafe Queenside game White = all (not . isAttacked game) [Coordinate 'b' 1, Coordinate 'c' 1, Coordinate 'd' 1]
isCastleSafe Queenside game Black = all (not . isAttacked game) [Coordinate 'b' 8, Coordinate 'c' 8, Coordinate 'd' 8]
