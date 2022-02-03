module Test.Placements where

import           Chess.Base
import           Chess.Bitboard
import           Chess.Board
import           Chess.Game

import           Control.Monad.State.Lazy

placePiece :: RegularGame -> Piece -> Coordinate -> RegularGame
placePiece g p c@(Coordinate f r) =
  g { placement = addPiece (placement g) (Just p) c }

setupGame :: [(Piece, Coordinate)] -> RegularGame
setupGame piecePositions = flip execState emptyTest $ setupGame' piecePositions where

  setupGame' :: [(Piece, Coordinate)] -> State RegularGame ()
  setupGame' []         = return ()
  setupGame' (pc : pcs) = do
    board <- get
    put $ uncurry (placePiece board) pc
    setupGame' pcs

diagonalTest :: (Game BitboardRepresentation)
diagonalTest = regularGameToBitboardGame $ setupGame
  [ (Piece Queen White, Coordinate 'f' 3)
  , (Piece Queen White, Coordinate 'f' 7)
  , (Piece Queen White, Coordinate 'd' 5)
  ]


discoveredCheckTest :: (Game BitboardRepresentation)
discoveredCheckTest = regularGameToBitboardGame $ setupGame
  [ (Piece King Black , Coordinate 'e' 8)
  , (Piece Pawn Black , Coordinate 'e' 5)
  , (Piece King White , Coordinate 'e' 1)
  , (Piece Queen White, Coordinate 'e' 2)
  ]

doubleCheckTest :: (Game BitboardRepresentation)
doubleCheckTest = regularGameToBitboardGame $ (setupGame
                    [ (Piece King Black  , Coordinate 'e' 8)
                    , (Piece Bishop Black, Coordinate 'h' 7)
                    , (Piece Knight White, Coordinate 'g' 7)
                    , (Piece Rook White  , Coordinate 'e' 4)
                    , (Piece King White  , Coordinate 'e' 1)
                    ]
                  )
  { activeColor = Black
  }

castleIntoCheckTest :: (Game BitboardRepresentation)
castleIntoCheckTest =
  regularGameToBitboardGame $ (setupGame
    [ (Piece King Black, Coordinate 'e' 8)
    , (Piece Rook Black, Coordinate 'g' 8)
    , (Piece Rook White, Coordinate 'h' 1)
    , (Piece King White, Coordinate 'e' 1)
    ]
  )

whiteKingsideCastleThroughCheckTest :: (Game BitboardRepresentation)
whiteKingsideCastleThroughCheckTest =
  regularGameToBitboardGame $ (setupGame
    [ (Piece King Black, Coordinate 'e' 8)
    , (Piece Rook Black, Coordinate 'f' 8)
    , (Piece Rook White, Coordinate 'h' 1)
    , (Piece King White, Coordinate 'e' 1)
    ]
  )

whiteQueensideCastleThroughCheckTest :: (Game BitboardRepresentation)
whiteQueensideCastleThroughCheckTest =
  regularGameToBitboardGame $ (setupGame
    [ (Piece King Black, Coordinate 'e' 8)
    , (Piece Rook Black, Coordinate 'd' 8)
    , (Piece Rook White, Coordinate 'a' 1)
    , (Piece King White, Coordinate 'e' 1)
    ]
  )

blackKingsideCastleThroughCheckTest :: (Game BitboardRepresentation)
blackKingsideCastleThroughCheckTest =
  regularGameToBitboardGame $ (setupGame
      [ (Piece King Black, Coordinate 'e' 8)
      , (Piece Rook Black, Coordinate 'h' 8)
      , (Piece Rook White, Coordinate 'f' 1)
      , (Piece King White, Coordinate 'e' 1)
      ]
    )
    { activeColor = Black
    }

blackQueensideCastleThroughCheckTest :: (Game BitboardRepresentation)
blackQueensideCastleThroughCheckTest =
  regularGameToBitboardGame $ (setupGame
      [ (Piece King Black, Coordinate 'e' 8)
      , (Piece Rook Black, Coordinate 'a' 8)
      , (Piece Rook White, Coordinate 'd' 1)
      , (Piece King White, Coordinate 'e' 1)
      ]
    )
    { activeColor = Black
    }

whitePromotionTest :: (Game BitboardRepresentation)
whitePromotionTest = regularGameToBitboardGame $ setupGame
  [ (Piece Pawn White, Coordinate 'e' 7)
  , (Piece King White, Coordinate 'a' 1)
  , (Piece King Black, Coordinate 'h' 1)
  ]

blackPromotionTest :: (Game BitboardRepresentation)
blackPromotionTest = regularGameToBitboardGame $ (setupGame
                       [ (Piece Pawn Black, Coordinate 'e' 2)
                       , (Piece King White, Coordinate 'a' 8)
                       , (Piece King Black, Coordinate 'h' 8)
                       ]
                     )
  { activeColor = Black
  }

promotionPinTest :: (Game BitboardRepresentation)
promotionPinTest = regularGameToBitboardGame $ setupGame
  [ (Piece King White, Coordinate 'h' 7)
  , (Piece Pawn White, Coordinate 'e' 7)
  , (Piece King Black, Coordinate 'a' 1)
  , (Piece Rook Black, Coordinate 'a' 7)
  ]

enPassantPinTest :: (Game BitboardRepresentation)
enPassantPinTest = regularGameToBitboardGame $ (setupGame
                     [ (Piece King White, Coordinate 'e' 1)
                     , (Piece Pawn White, Coordinate 'e' 5)
                     , (Piece Pawn Black, Coordinate 'd' 5)
                     , (Piece King Black, Coordinate 'e' 8)
                     , (Piece Rook Black, Coordinate 'e' 7)
                     ]
                   )
  { enPassantSquare = Just $ Coordinate 'd' 6
  }


emptyTest :: RegularGame
emptyTest = Game
  { placement = [ [ Square { pieceOn = Nothing, location = Coordinate 'a' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 1 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 1 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 2 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 2 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 3 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 3 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 4 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 4 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 5 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 5 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 6 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 6 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 7 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 7 }
                  ]
                , [ Square { pieceOn = Nothing, location = Coordinate 'a' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'b' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'c' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'd' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'e' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'f' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'g' 8 }
                  , Square { pieceOn = Nothing, location = Coordinate 'h' 8 }
                  ]
                ]
  , activeColor     = White
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

startingPos :: (Game BitboardRepresentation)
startingPos = regularGameToBitboardGame $ regularStartingPos

regularStartingPos :: RegularGame
regularStartingPos = Game
  { placement       =
    [ [ Square { pieceOn  = Just (Piece Rook White)
               , location = Coordinate 'a' 1
               }
      , Square { pieceOn  = Just (Piece Knight White)
               , location = Coordinate 'b' 1
               }
      , Square { pieceOn  = Just (Piece Bishop White)
               , location = Coordinate 'c' 1
               }
      , Square { pieceOn  = Just (Piece Queen White)
               , location = Coordinate 'd' 1
               }
      , Square { pieceOn  = Just (Piece King White)
               , location = Coordinate 'e' 1
               }
      , Square { pieceOn  = Just (Piece Bishop White)
               , location = Coordinate 'f' 1
               }
      , Square { pieceOn  = Just (Piece Knight White)
               , location = Coordinate 'g' 1
               }
      , Square { pieceOn  = Just (Piece Rook White)
               , location = Coordinate 'h' 1
               }
      ]
    , [ Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'a' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'b' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'c' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'd' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'e' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'f' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'g' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'h' 2
               }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 3 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 4 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 5 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 6 }
      ]
    , [ Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'a' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'b' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'c' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'd' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'e' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'f' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'g' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'h' 7
               }
      ]
    , [ Square { pieceOn  = Just (Piece Rook Black)
               , location = Coordinate 'a' 8
               }
      , Square { pieceOn  = Just (Piece Knight Black)
               , location = Coordinate 'b' 8
               }
      , Square { pieceOn  = Just (Piece Bishop Black)
               , location = Coordinate 'c' 8
               }
      , Square { pieceOn  = Just (Piece Queen Black)
               , location = Coordinate 'd' 8
               }
      , Square { pieceOn  = Just (Piece King Black)
               , location = Coordinate 'e' 8
               }
      , Square { pieceOn  = Just (Piece Bishop Black)
               , location = Coordinate 'f' 8
               }
      , Square { pieceOn  = Just (Piece Knight Black)
               , location = Coordinate 'g' 8
               }
      , Square { pieceOn  = Just (Piece Rook Black)
               , location = Coordinate 'h' 8
               }
      ]
    ]
  , activeColor     = White
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

kingOpening :: (Game BitboardRepresentation)
kingOpening = regularGameToBitboardGame $ Game
  { placement       =
    [ [ Square { pieceOn  = Just (Piece Rook White)
               , location = Coordinate 'a' 1
               }
      , Square { pieceOn  = Just (Piece Knight White)
               , location = Coordinate 'b' 1
               }
      , Square { pieceOn  = Just (Piece Bishop White)
               , location = Coordinate 'c' 1
               }
      , Square { pieceOn  = Just (Piece Queen White)
               , location = Coordinate 'd' 1
               }
      , Square { pieceOn  = Just (Piece King White)
               , location = Coordinate 'e' 1
               }
      , Square { pieceOn  = Just (Piece Bishop White)
               , location = Coordinate 'f' 1
               }
      , Square { pieceOn  = Just (Piece Knight White)
               , location = Coordinate 'g' 1
               }
      , Square { pieceOn  = Just (Piece Rook White)
               , location = Coordinate 'h' 1
               }
      ]
    , [ Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'a' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'b' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'c' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'd' 2
               }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 2 }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'f' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'g' 2
               }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'h' 2
               }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 3 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 3 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 4 }
      , Square { pieceOn  = Just (Piece Pawn White)
               , location = Coordinate 'e' 4
               }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 4 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 4 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 5 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 5 }
      ]
    , [ Square { pieceOn = Nothing, location = Coordinate 'a' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'b' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'c' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'd' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'e' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'f' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'g' 6 }
      , Square { pieceOn = Nothing, location = Coordinate 'h' 6 }
      ]
    , [ Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'a' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'b' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'c' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'd' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'e' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'f' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'g' 7
               }
      , Square { pieceOn  = Just (Piece Pawn Black)
               , location = Coordinate 'h' 7
               }
      ]
    , [ Square { pieceOn  = Just (Piece Rook Black)
               , location = Coordinate 'a' 8
               }
      , Square { pieceOn  = Just (Piece Knight Black)
               , location = Coordinate 'b' 8
               }
      , Square { pieceOn  = Just (Piece Bishop Black)
               , location = Coordinate 'c' 8
               }
      , Square { pieceOn  = Just (Piece Queen Black)
               , location = Coordinate 'd' 8
               }
      , Square { pieceOn  = Just (Piece King Black)
               , location = Coordinate 'e' 8
               }
      , Square { pieceOn  = Just (Piece Bishop Black)
               , location = Coordinate 'f' 8
               }
      , Square { pieceOn  = Just (Piece Knight Black)
               , location = Coordinate 'g' 8
               }
      , Square { pieceOn  = Just (Piece Rook Black)
               , location = Coordinate 'h' 8
               }
      ]
    ]
  , activeColor     = Black
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }
