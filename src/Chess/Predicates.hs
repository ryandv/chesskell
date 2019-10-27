module Chess.Predicates
  ( isAttacked
  , isChecked
  , isCheckmate
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
isAttacked :: RegularGame -> Coordinate -> Bool
isAttacked game coord = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where

  nextState = (placement game)
  bitboard = regularToBitboard $ placement game

  activePly = (activeColor game)

  placeGhostPiece :: PieceType -> RegularGame
  placeGhostPiece pt = game { placement = addPiece nextState (Just (Piece pt activePly)) coord }

  isQueenChecking :: Bool
  isQueenChecking = isChecking (placeGhostPiece Queen) coord Queen

  isRookChecking :: Bool
  isRookChecking = fastIsChecking potentialRookMoves bitboard activePly coord Rook

  moveIsCapture :: Move -> Bool
  moveIsCapture = (== Capture) . moveType

  moveMatchesPieceType    :: PieceType -> Move -> Bool
  moveMatchesPieceType pt = (== (Just pt)) . (fmap pieceType) . bitboardPieceAt bitboard . moveTo

  moveIsFromTargetCoordinate :: Move -> Bool
  moveIsFromTargetCoordinate = (== coord) . moveFrom

  isBishopChecking :: Bool
  isBishopChecking = fastIsChecking potentialBishopMoves bitboard activePly coord Bishop

  isKnightChecking :: Bool
  isKnightChecking = fastIsChecking potentialKnightMoves bitboard activePly coord Knight

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = fastIsChecking (potentialPawnMoves (enPassantSquare game)) bitboard activePly coord Pawn

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = fastIsChecking (potentialKingMoves (castlingRights game)) bitboard activePly coord King

isChecked      :: RegularGame -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare ply = location $ head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] (placement game)

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate          :: RegularGame -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

moveIsLegal :: Move -> RegularGame -> Bool
moveIsLegal move@Move{ moveFrom = from } game = moveIsPseudoLegal && moveIsByRightPlayer && (notCheckedAfterMove game move) where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (pieceAt position from)) == (Just (activeColor game))
  position = placement game

notCheckedAfterMove :: RegularGame -> Move -> Bool
notCheckedAfterMove game move = (not $ isChecked game { placement = positionAfterMove (placement game) move})

noLegalMovesRemaining :: RegularGame -> Player -> Bool
noLegalMovesRemaining game ply = null
                               $ filter (liftA2 (&&) pieceIsOwnedByPly (notCheckedAfterMove game))
                               $ pseudoLegalMoves game where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceAt (placement game) from)) == (Just ply)

fastIsChecking :: (BitboardRepresentation -> Player -> Coordinate -> [Move]) -> BitboardRepresentation -> Player -> Coordinate -> PieceType -> Bool
fastIsChecking moveGenerator bitboard activePly coord pt = not
                                                         $ null
                                                         $ filter (liftA2 (&&) moveIsCapture (moveMatchesPieceType pt))
                                                         $ filter moveIsFromTargetCoordinate
                                                         $ moveGenerator bitboard activePly coord

  where moveIsCapture :: Move -> Bool
        moveIsCapture = (== Capture) . moveType

        moveMatchesPieceType    :: PieceType -> Move -> Bool
        moveMatchesPieceType pt = (== (Just pt)) . (fmap pieceType) . bitboardPieceAt bitboard . moveTo

        moveIsFromTargetCoordinate :: Move -> Bool
        moveIsFromTargetCoordinate = (== coord) . moveFrom

isChecking                             :: RegularGame -> Coordinate -> PieceType -> Bool
isChecking gameWithGhostPiece coord pt = not
                                       $ null
                                       $ filter (liftA2 (&&) moveIsCapture moveMatchesPieceType)
                                       $ filter moveIsFromTargetCoordinate
                                       $ pseudoLegalMoves gameWithGhostPiece where

  moveIsCapture :: Move -> Bool
  moveIsCapture = (== Capture) . moveType

  moveMatchesPieceType :: Move -> Bool
  moveMatchesPieceType = (== pt) . fromJust . (fmap pieceType) . pieceAt (placement gameWithGhostPiece) . moveTo

  moveIsFromTargetCoordinate :: Move -> Bool
  moveIsFromTargetCoordinate = (== coord) . moveFrom
