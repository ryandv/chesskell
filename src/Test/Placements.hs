module Test.Placements where

import Chess.Base

rookTest :: RegularGame
rookTest = RegularGame
  { placement =
    [
      [ Square { pieceOn = Just (Piece King White)
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
      , Square { pieceOn = Just (Piece Rook White)
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
      , Square { pieceOn = Just (Piece Rook White)
               , location = Coordinate 'd' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 5
               }
      , Square { pieceOn = Just (Piece Rook White)
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
      , Square { pieceOn = Just (Piece King Black)
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = White
  , castlingRights  = (CastleRights True True True True)
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

emptyTest :: RegularGame
emptyTest = RegularGame
  { placement =
    [
      [ Square { pieceOn = Just (Piece King White)
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
      , Square { pieceOn = Just (Piece Rook White)
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
      , Square { pieceOn = Just (Piece King Black)
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = White
  , castlingRights  = (CastleRights True True True True)
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

diagonalTest :: RegularGame
diagonalTest = RegularGame
  { placement =
    [
      [ Square { pieceOn = Just (Piece King White)
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
      , Square { pieceOn = Just (Piece Queen White)
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
      , Square { pieceOn = Just (Piece Queen White)
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
      , Square { pieceOn = Just (Piece Queen White)
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
      , Square { pieceOn = Just (Piece King Black)
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = White
  , castlingRights  = (CastleRights True True True True)
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

