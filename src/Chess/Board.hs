module Chess.Board where

import Chess.Base

positionAfterMove :: RegularBoardRepresentation -> Move -> RegularBoardRepresentation
positionAfterMove position move@Move { moveFrom = from } = movePiece position (pieceAt position from) move

movePiece :: RegularBoardRepresentation -> Maybe Piece -> Move -> RegularBoardRepresentation
movePiece position piece Move { moveFrom = from
                              , moveTo   = to } = addPiece (addPiece position Nothing from) piece to

addPiece                        :: RegularBoardRepresentation -> Maybe Piece -> Coordinate -> RegularBoardRepresentation
addPiece b p c@(Coordinate f r) = newPlacement where
  newPlacement = fst splitBoard ++ [fst splitRank ++ [Square p c] ++ (tail . snd $ splitRank)] ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) b
  splitRank = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard
