module Chess.Predicates where

import Chess.Base
import Chess.Board
import Chess.MoveGen
import Chess.MoveGen.Bishop
import Chess.MoveGen.King
import Chess.MoveGen.Knight
import Chess.MoveGen.Pawn
import Chess.MoveGen.Queen
import Chess.MoveGen.Rook

import Control.Applicative

import Data.Maybe

moveIsLegal :: Move -> RegularGame -> Bool
moveIsLegal move@Move{ moveFrom = from} game = moveIsPseudoLegal && moveIsByRightPlayer && notCheckedAfterMove where

  moveIsPseudoLegal = (move `elem` pseudoLegalMoves game)
  moveIsByRightPlayer = (pieceOwner <$> (pieceOn $ (squareAt position from))) == (Just (activeColor game))
  notCheckedAfterMove = (not $ isChecked game { placement = positionAfterMove position move})
  position = placement game

noLegalMovesRemaining :: RegularGame -> Player -> Bool
noLegalMovesRemaining game ply = (null $ filter (\x -> pieceIsOwnedByPly x && (not $ isChecked game { placement = positionAfterMove (placement game) x })) $ pseudoLegalMoves game) where

  pieceIsOwnedByPly :: Move -> Bool
  pieceIsOwnedByPly Move { moveFrom = from } = (pieceOwner <$> (pieceOn $ (squareAt (placement game) from))) == (Just ply)

isCheckmate          :: RegularGame -> Player -> Bool
isCheckmate game ply = (isChecked game) && noLegalMovesRemaining game ply

isStalemate          :: RegularGame -> Player -> Bool
isStalemate game ply = (not $ isChecked game) && noLegalMovesRemaining game ply

-- Given an opponent's PieceType, this function works by placing down a ghost piece of the same PieceType but the active player's color,
-- and checks to see if the ghost piece can capture any of the opponent's pieces.
isChecking            :: RegularGame -> Coordinate -> PieceType -> (RegularGame -> Coordinate -> [Move]) -> Bool
isChecking gameWithGhostPiece sq pt movegen = not
                                            $ null
                                            $ filter (liftA2 (&&) moveIsCapture moveMatchesPieceType)
                                            $ movegen gameWithGhostPiece sq where

  moveIsCapture :: Move -> Bool
  moveIsCapture = (== Capture) . moveType

  moveMatchesPieceType :: Move -> Bool
  moveMatchesPieceType = (== pt) . fromJust . (fmap pieceType) . pieceOn . squareAt (placement gameWithGhostPiece) . moveTo

isAttacked :: RegularGame -> Coordinate -> Bool
isAttacked game sq = isQueenChecking || isRookChecking || isBishopChecking || isKnightChecking || isPawnChecking || isKingChecking where

  nextState = (placement game)

  activePly = (activeColor game)

  placeGhostPiece :: PieceType -> RegularGame
  placeGhostPiece pt = game { placement = addPiece nextState (Just (Piece pt activePly)) sq }

  isQueenChecking :: Bool
  isQueenChecking = isChecking (placeGhostPiece Queen) sq Queen potentialQueenMoves

  isRookChecking :: Bool
  isRookChecking = isChecking (placeGhostPiece Rook) sq Rook potentialRookMoves

  isBishopChecking :: Bool
  isBishopChecking = isChecking (placeGhostPiece Bishop) sq Bishop potentialBishopMoves

  isKnightChecking :: Bool
  isKnightChecking = isChecking (placeGhostPiece Knight) sq Knight potentialKnightMoves

  -- TODO: do we need to consider en passant? I think not.
  isPawnChecking :: Bool
  isPawnChecking = isChecking (placeGhostPiece Pawn) sq Pawn potentialPawnMoves

  -- TODO: do we need to consider castling? I think not.
  isKingChecking :: Bool
  isKingChecking = isChecking (placeGhostPiece King) sq King potentialKingMoves

isChecked      :: RegularGame -> Bool
isChecked game = isAttacked game (kingSquare (activeColor game)) where

  kingSquare     :: Player -> Coordinate
  kingSquare ply = location $ head $ filter ((== Just (Piece King ply)) . pieceOn) $ foldr (++) [] (placement game)
