module Test.Placements.MoveGen.Rook where

import Chess.Base
import Chess.Game

import Test.Placements

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
