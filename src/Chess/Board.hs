module Chess.Board where

import           Chess.Base

positionAfterMove
  :: RegularBoardRepresentation -> Move -> RegularBoardRepresentation
positionAfterMove position move@(Move from _) =
  movePiece position (pieceAt position from) move
positionAfterMove position move@(Capture from _) =
  movePiece position (pieceAt position from) move
positionAfterMove position move@(Castle from _) =
  movePiece position (pieceAt position from) move
positionAfterMove position move@(EnPassant from _) =
  movePiece position (pieceAt position from) move
positionAfterMove position move@(Promote from _ _) =
  movePiece position (pieceAt position from) move

movePiece
  :: RegularBoardRepresentation
  -> Maybe Piece
  -> Move
  -> RegularBoardRepresentation
movePiece position piece (Move from to) =
  addPiece (addPiece position Nothing from) piece to
movePiece position piece (Capture from to) =
  addPiece (addPiece position Nothing from) piece to
movePiece position piece (Castle from to) =
  addPiece (addPiece position Nothing from) piece to
movePiece position piece (EnPassant from to) =
  addPiece (addPiece position Nothing from) piece to
movePiece position piece (Promote from to _) =
  addPiece (addPiece position Nothing from) piece to

addPiece
  :: RegularBoardRepresentation
  -> Maybe Piece
  -> Coordinate
  -> RegularBoardRepresentation
addPiece b p c@(Coordinate f r) = newPlacement where
  newPlacement =
    fst splitBoard
      ++ [fst splitRank ++ [Square p c] ++ (tail . snd $ splitRank)]
      ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) b
  splitRank  = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard

pieceAt :: RegularBoardRepresentation -> Coordinate -> Maybe Piece
pieceAt b (Coordinate f r) =
  pieceOn $ (b !! (r - 1)) !! (fromEnum f - fromEnum 'a')
