module Test.Placements.MoveGen.Knight where

import Chess.Base
import Chess.Game

import Test.Placements

onlyKnightTest :: RegularGame
onlyKnightTest = setupGame [ (Piece Knight White, Coordinate 'd' 4) ]

knightTest :: RegularGame
knightTest = setupGame [ (Piece Knight White, Coordinate 'd' 4)
                       , (Piece Knight White, Coordinate 'b' 5)
                       , (Piece Knight White, Coordinate 'f' 3)
                       ]
