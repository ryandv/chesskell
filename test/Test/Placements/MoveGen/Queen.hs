module Test.Placements.MoveGen.Queen where

import Chess.Base
import Chess.Game

import Test.Placements

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
