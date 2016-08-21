module Test.Placements.King where

import Chess.Base
import Chess.Game

import Test.Placements

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
