module Test.Placements.MoveGen.Bishop where

import Chess.Base
import Chess.Game

import Test.Placements

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

