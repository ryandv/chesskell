module Test.Placements where

import Chess.Base
import Chess.Game

import Control.Monad.State.Lazy

placePiece                        :: RegularGame -> Piece -> Coordinate -> RegularGame
placePiece g p c@(Coordinate f r) = g { placement = addPiece (placement g) (Just p) c }

setupGame                :: [(Piece, Coordinate)] -> RegularGame
setupGame piecePositions = flip execState emptyTest $ setupGame' piecePositions where

  setupGame'                :: [(Piece, Coordinate)] -> State RegularGame ()
  setupGame' []             = return ()
  setupGame' (pc:pcs)       = do
    board <- get
    put $ uncurry (placePiece board) pc
    setupGame' pcs

rookTest :: RegularGame
rookTest = setupGame [ (Piece Rook White, Coordinate 'd' 3)
                     , (Piece Rook White, Coordinate 'd' 5)
                     , (Piece Rook White, Coordinate 'f' 5)
                     ]

onlyRookTest :: RegularGame
onlyRookTest = setupGame [ (Piece Rook White, Coordinate 'd' 5) ]

rookCaptureVerticalTest :: RegularGame
rookCaptureVerticalTest = setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                                    , (Piece Rook White, Coordinate 'd' 5)
                                    ]

rookCaptureHorizontalTest :: RegularGame
rookCaptureHorizontalTest = setupGame [ (Piece Pawn Black, Coordinate 'f' 5)
                                      , (Piece Rook White, Coordinate 'd' 5)
                                      ]

rookAllCapturesTest :: RegularGame
rookAllCapturesTest = setupGame [ (Piece Pawn Black, Coordinate 'd' 7)
                                , (Piece Pawn Black, Coordinate 'd' 3)
                                , (Piece Pawn Black, Coordinate 'b' 5)
                                , (Piece Pawn Black, Coordinate 'f' 5)
                                , (Piece Rook White, Coordinate 'd' 5)
                                ]

bishopAllCapturesTest :: RegularGame
bishopAllCapturesTest = setupGame [ (Piece Pawn Black, Coordinate 'c' 7)
                                  , (Piece Pawn Black, Coordinate 'g' 7)
                                  , (Piece Pawn Black, Coordinate 'g' 3)
                                  , (Piece Pawn Black, Coordinate 'c' 3)
                                  , (Piece Bishop White, Coordinate 'e' 5)
                                  ]

bishopCaptureTest :: RegularGame
bishopCaptureTest = setupGame [ (Piece Pawn Black, Coordinate 'd' 4)
                              , (Piece Bishop White, Coordinate 'a' 1)
                              ]

bishopTest :: RegularGame
bishopTest = setupGame [ (Piece Pawn White, Coordinate 'c' 3)
                       , (Piece Pawn White, Coordinate 'g' 7)
                       , (Piece Bishop White, Coordinate 'e' 5)
                       ]

onlyBishopTest :: RegularGame
onlyBishopTest = setupGame [ (Piece Bishop White, Coordinate 'e' 5) ]

diagonalTest :: RegularGame
diagonalTest = setupGame [ (Piece Queen White, Coordinate 'f' 3)
                         , (Piece Queen White, Coordinate 'f' 7)
                         , (Piece Queen White, Coordinate 'd' 5)
                         ]

onlyKnightTest :: RegularGame
onlyKnightTest = setupGame [ (Piece Knight White, Coordinate 'd' 4) ]

knightTest :: RegularGame
knightTest = setupGame [ (Piece Knight White, Coordinate 'd' 4)
                       , (Piece Knight White, Coordinate 'b' 5)
                       , (Piece Knight White, Coordinate 'f' 3)
                       ]

onlyQueenTest :: RegularGame
onlyQueenTest = setupGame [ (Piece Queen White, Coordinate 'd' 4) ]

queenTest :: RegularGame
queenTest = setupGame [ (Piece Queen White, Coordinate 'd' 4)
                      , (Piece Rook White, Coordinate 'd' 7)
                      , (Piece Bishop White, Coordinate 'g' 7)
                      ]

queenCaptureTest :: RegularGame
queenCaptureTest = setupGame [ (Piece Queen White, Coordinate 'd' 4)
                             , (Piece Rook Black, Coordinate 'd' 7)
                             , (Piece Bishop Black, Coordinate 'g' 7)
                             ]

onlyKingTest :: RegularGame
onlyKingTest = setupGame [ (Piece King White, Coordinate 'd' 4) ]

whiteKingOOTest :: RegularGame
whiteKingOOTest = setupGame [ (Piece King White, Coordinate 'e' 1)
                            , (Piece Rook White, Coordinate 'h' 1)
                            ]

whiteKingMovedNoOOTest :: RegularGame
whiteKingMovedNoOOTest = setupGame [ (Piece King White, Coordinate 'd' 4)
                                   , (Piece Rook White, Coordinate 'h' 1)
                                   ]

whiteKingNoRookCastleTest :: RegularGame
whiteKingNoRookCastleTest = setupGame [ (Piece King White, Coordinate 'e' 1) ]

whiteKingOOOTest :: RegularGame
whiteKingOOOTest = setupGame [ (Piece King White, Coordinate 'e' 1)
                             , (Piece Rook White, Coordinate 'a' 1)
                             ]

whiteKingMovedNoOOOTest :: RegularGame
whiteKingMovedNoOOOTest = setupGame [ (Piece King White, Coordinate 'd' 4)
                                    , (Piece Rook White, Coordinate 'a' 1)
                                    ]

whiteKingBothCastlesTest :: RegularGame
whiteKingBothCastlesTest = setupGame [ (Piece King White, Coordinate 'e' 1)
                                     , (Piece Rook White, Coordinate 'a' 1)
                                     , (Piece Rook White, Coordinate 'h' 1)
                                     ]

blackKingOOTest :: RegularGame
blackKingOOTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                             , (Piece Rook Black, Coordinate 'h' 8)
                             ]) { activeColor = Black }

blackKingMovedNoOOTest :: RegularGame
blackKingMovedNoOOTest = setupGame [ (Piece King Black, Coordinate 'd' 4)
                                   , (Piece Rook Black, Coordinate 'h' 8)
                                   ]

blackKingNoRookCastleTest :: RegularGame
blackKingNoRookCastleTest = setupGame [ (Piece King Black, Coordinate 'e' 8) ]

blackKingOOOTest :: RegularGame
blackKingOOOTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                              , (Piece Rook Black, Coordinate 'a' 8)
                              ]) { activeColor = Black }

blackKingMovedNoOOOTest :: RegularGame
blackKingMovedNoOOOTest = setupGame [ (Piece King Black, Coordinate 'd' 4)
                                    , (Piece Rook Black, Coordinate 'a' 8)
                                    ]

blackKingBothCastlesTest :: RegularGame
blackKingBothCastlesTest = setupGame [ (Piece King Black, Coordinate 'e' 8)
                                     , (Piece Rook Black, Coordinate 'a' 8)
                                     , (Piece Rook Black, Coordinate 'h' 8)
                                     ]

whiteEnPassantTest :: RegularGame
whiteEnPassantTest = (setupGame [ (Piece Pawn Black, Coordinate 'd' 5)
                                , (Piece Pawn White, Coordinate 'e' 5)
                                , (Piece King White, Coordinate 'a' 1)
                                , (Piece King Black, Coordinate 'h' 1)
                                ]) { enPassantSquare = Just $ Coordinate 'd' 6 }

blackEnPassantTest :: RegularGame
blackEnPassantTest = (setupGame [ (Piece Pawn Black, Coordinate 'd' 4)
                                , (Piece Pawn White, Coordinate 'e' 4)
                                , (Piece King White, Coordinate 'a' 1)
                                , (Piece King Black, Coordinate 'h' 1)
                                ]) { enPassantSquare = Just $ Coordinate 'e' 3 }

discoveredCheckTest :: RegularGame
discoveredCheckTest = setupGame [ (Piece King Black, Coordinate 'e' 8)
                                , (Piece Pawn Black, Coordinate 'e' 5)
                                , (Piece King White, Coordinate 'e' 1)
                                , (Piece Queen White, Coordinate 'e' 2)
                                ]

doubleCheckTest :: RegularGame
doubleCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                             , (Piece Bishop Black, Coordinate 'h' 7)
                             , (Piece Knight White, Coordinate 'g' 7)
                             , (Piece Rook White, Coordinate 'e' 4)
                             , (Piece King White, Coordinate 'e' 1)
                             ]) { activeColor = Black }

castleIntoCheckTest :: RegularGame
castleIntoCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                 , (Piece Rook Black, Coordinate 'g' 8)
                                 , (Piece Rook White, Coordinate 'h' 1)
                                 , (Piece King White, Coordinate 'e' 1)])

whiteKingsideCastleThroughCheckTest :: RegularGame
whiteKingsideCastleThroughCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                                 , (Piece Rook Black, Coordinate 'f' 8)
                                                 , (Piece Rook White, Coordinate 'h' 1)
                                                 , (Piece King White, Coordinate 'e' 1)])

whiteQueensideCastleThroughCheckTest :: RegularGame
whiteQueensideCastleThroughCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                                  , (Piece Rook Black, Coordinate 'd' 8)
                                                  , (Piece Rook White, Coordinate 'a' 1)
                                                  , (Piece King White, Coordinate 'e' 1)])

blackKingsideCastleThroughCheckTest :: RegularGame
blackKingsideCastleThroughCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                                 , (Piece Rook Black, Coordinate 'h' 8)
                                                 , (Piece Rook White, Coordinate 'f' 1)
                                                 , (Piece King White, Coordinate 'e' 1)])

blackQueensideCastleThroughCheckTest :: RegularGame
blackQueensideCastleThroughCheckTest = (setupGame [ (Piece King Black, Coordinate 'e' 8)
                                                  , (Piece Rook Black, Coordinate 'a' 8)
                                                  , (Piece Rook White, Coordinate 'd' 1)
                                                  , (Piece King White, Coordinate 'e' 1)])

whitePromotionTest :: RegularGame
whitePromotionTest = setupGame [ (Piece Pawn White, Coordinate 'e' 7)
                               , (Piece King White, Coordinate 'a' 1)
                               , (Piece King Black, Coordinate 'h' 1)
                               ]

blackPromotionTest :: RegularGame
blackPromotionTest = (setupGame [ (Piece Pawn Black, Coordinate 'e' 2)
                                , (Piece King White, Coordinate 'a' 8)
                                , (Piece King Black, Coordinate 'h' 8)
                                ]) { activeColor = Black }

promotionPinTest :: RegularGame
promotionPinTest = setupGame [ (Piece King White, Coordinate 'h' 7)
                              , (Piece Pawn White, Coordinate 'e' 7)
                              , (Piece King Black, Coordinate 'a' 1)
                              , (Piece Rook Black, Coordinate 'a' 7)]

enPassantPinTest :: RegularGame
enPassantPinTest = (setupGame [ (Piece King White, Coordinate 'e' 1)
                              , (Piece Pawn White, Coordinate 'e' 5)
                              , (Piece Pawn Black, Coordinate 'd' 5)
                              , (Piece King Black, Coordinate 'e' 8)
                              , (Piece Rook Black, Coordinate 'e' 7)]) { enPassantSquare = Just $ Coordinate 'd' 6 }


emptyTest :: RegularGame
emptyTest = Game
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

startingPos :: RegularGame
startingPos = Game
  { placement =
    [
      [ Square { pieceOn = Just (Piece Rook White)
               , location = Coordinate 'a' 1
               }
      , Square { pieceOn = Just (Piece Knight White)
               , location = Coordinate 'b' 1
               }
      , Square { pieceOn = Just (Piece Bishop White)
               , location = Coordinate 'c' 1
               }
      , Square { pieceOn = Just (Piece Queen White)
               , location = Coordinate 'd' 1
               }
      , Square { pieceOn = Just (Piece King White)
               , location = Coordinate 'e' 1
               }
      , Square { pieceOn = Just (Piece Bishop White)
               , location = Coordinate 'f' 1
               }
      , Square { pieceOn = Just (Piece Knight White)
               , location = Coordinate 'g' 1
               }
      , Square { pieceOn = Just (Piece Rook White)
               , location = Coordinate 'h' 1
               }
      ]

    , [ Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'a' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'b' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'c' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'd' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'e' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'f' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
               , location = Coordinate 'g' 2
               }
      , Square { pieceOn = Just (Piece Pawn White)
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
    , [ Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'a' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'b' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'c' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'd' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'e' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'f' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'g' 7
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'h' 7}
               ]
    , [ Square { pieceOn = Just (Piece Rook Black)
               , location = Coordinate 'a' 8
               }
      , Square { pieceOn = Just (Piece Knight Black)
               , location = Coordinate 'b' 8
               }
      , Square { pieceOn = Just (Piece Bishop Black)
               , location = Coordinate 'c' 8
               }
      , Square { pieceOn = Just (Piece Queen Black)
               , location = Coordinate 'd' 8
               }
      , Square { pieceOn = Just (Piece King Black)
               , location = Coordinate 'e' 8
               }
      , Square { pieceOn = Just (Piece Bishop Black)
               , location = Coordinate 'f' 8
               }
      , Square { pieceOn = Just (Piece Knight Black)
               , location = Coordinate 'g' 8
               }
      , Square { pieceOn = Just (Piece Rook Black)
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = White
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }
