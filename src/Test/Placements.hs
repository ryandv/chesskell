module Test.Placements where

import Chess.Base

placePiece                        :: RegularGame -> Piece -> Coordinate -> RegularGame
placePiece g p c@(Coordinate f r) = g { placement = newPlacement } where
  newPlacement = fst splitBoard ++ [fst splitRank ++ [Square (Just p) c] ++ (tail . snd $ splitRank)] ++ (tail . snd $ splitBoard)
  splitBoard = splitAt (r - 1) $ placement g
  splitRank = splitAt (fromEnum f - fromEnum 'a') targetRank
  targetRank = head . snd $ splitBoard

rookTest :: RegularGame
rookTest = placePiece
             (placePiece
               (placePiece emptyTest (Piece Rook White) (Coordinate 'd' 3))
               (Piece Rook White) (Coordinate 'd' 5)) (Piece Rook White) (Coordinate 'f' 5)

onlyRookTest :: RegularGame
onlyRookTest = placePiece emptyTest (Piece Rook White) (Coordinate 'd' 5)

rookCaptureVerticalTest :: RegularGame
rookCaptureVerticalTest = placePiece
                    (placePiece emptyTest (Piece Pawn Black) (Coordinate 'd' 7))
                    (Piece Rook White) (Coordinate 'd' 5)

rookCaptureHorizontalTest :: RegularGame
rookCaptureHorizontalTest = placePiece
                    (placePiece emptyTest (Piece Pawn Black) (Coordinate 'f' 5))
                    (Piece Rook White) (Coordinate 'd' 5)

rookAllCapturesTest :: RegularGame
rookAllCapturesTest = placePiece
                        (placePiece
                          (placePiece
                            (placePiece
                              (placePiece emptyTest (Piece Pawn Black) (Coordinate 'd' 7))
                              (Piece Pawn Black) (Coordinate 'd' 3))
                            (Piece Pawn Black) (Coordinate 'b' 5))
                          (Piece Pawn Black) (Coordinate 'f' 5))
                        (Piece Rook White) (Coordinate 'd' 5)

bishopAllCapturesTest :: RegularGame
bishopAllCapturesTest = placePiece
                        (placePiece
                          (placePiece
                            (placePiece
                              (placePiece emptyTest (Piece Pawn Black) (Coordinate 'c' 7))
                              (Piece Pawn Black) (Coordinate 'g' 7))
                            (Piece Pawn Black) (Coordinate 'g' 3))
                          (Piece Pawn Black) (Coordinate 'c' 3))
                        (Piece Bishop White) (Coordinate 'e' 5)

bishopCaptureTest :: RegularGame
bishopCaptureTest = placePiece
                    (placePiece emptyTest (Piece Pawn Black) (Coordinate 'd' 4))
                    (Piece Bishop White) (Coordinate 'a' 1)

bishopTest :: RegularGame
bishopTest = placePiece
               (placePiece
                 (placePiece emptyTest (Piece Pawn White) (Coordinate 'c' 3))
                 (Piece Bishop White) (Coordinate 'e' 5)) (Piece Pawn White) (Coordinate 'g' 7)

onlyBishopTest :: RegularGame
onlyBishopTest = placePiece emptyTest (Piece Bishop White) (Coordinate 'e' 5)

diagonalTest :: RegularGame
diagonalTest = placePiece
                 (placePiece
                   (placePiece emptyTest (Piece Queen White) (Coordinate 'f' 3))
                   (Piece Queen White) (Coordinate 'd' 5)) (Piece Queen White) (Coordinate 'f' 7)

emptyTest :: RegularGame
emptyTest = RegularGame
  { placement =
    [
      [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 1
               }
      ]

    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 2
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 2
               }
      ]

    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 3
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 3
               }
      ]
    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 4
               }
      ]
    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 5
               }
      ]
    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 6
               }
      ]
    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 7}
               ]
    , [ Square { pieceOn = Nothing
               , location = Coordinate 'a' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = White
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }
