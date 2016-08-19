module Test.Placements.MoveGen.Pawn where

import Chess.Base
import Chess.Game

import Test.Placements

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
