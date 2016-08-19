module Test.Chess.FenParserSpec where

import Chess.Base
import Chess.FenParser

import Test.Hspec
import Test.Placements

successful (Right x) = x

operaGame :: RegularGame
operaGame = Game
  { placement =
    [
      [ Square { pieceOn = Just (Piece Rook White)
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
      , Square { pieceOn = Just (Piece King White)
               , location = Coordinate 'e' 1
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 1
               }
      , Square { pieceOn = Nothing
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
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 2
               }
      , Square { pieceOn = Nothing
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
      , Square { pieceOn = Just (Piece Queen White)
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
      , Square { pieceOn = Just (Piece Bishop White)
               , location = Coordinate 'c' 4
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 4
               }
      , Square { pieceOn = Just (Piece Pawn White)
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
      , Square { pieceOn = Just (Piece Knight White)
               , location = Coordinate 'b' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 5
               }
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'e' 5
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'f' 5
               }
      , Square { pieceOn = Just (Piece Bishop White)
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
      , Square { pieceOn = Just (Piece Pawn Black)
               , location = Coordinate 'c' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 6
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'e' 6
               }
      , Square { pieceOn = Just (Piece Knight Black)
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
      , Square { pieceOn = Nothing
               , location = Coordinate 'b' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 7
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 7
               }
      , Square { pieceOn = Just (Piece Queen Black)
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
      , Square { pieceOn = Nothing
               , location = Coordinate 'c' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'd' 8
               }
      , Square { pieceOn = Just (Piece King Black)
               , location = Coordinate 'e' 8
               }
      , Square { pieceOn = Just (Piece Bishop Black)
               , location = Coordinate 'f' 8
               }
      , Square { pieceOn = Nothing
               , location = Coordinate 'g' 8
               }
      , Square { pieceOn = Just (Piece Rook Black)
               , location = Coordinate 'h' 8}
      ]
    ]
  , activeColor     = Black
  , castlingRights  = (CastleRights True True True True)
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 10
  }

spec :: Spec
spec =
  describe "FEN string parsing" $ do
    it "parses the starting position correctly" $
      (successful $ parseFen "" "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") `shouldBe` startingPos

    it "parses this position from the Opera Game" $
      (successful $ parseFen "" "rn2kb1r/p3qppp/2p2n2/1N2p1B1/2B1P3/1Q6/PPP2PPP/R3K2R b KQkq - 0 10") `shouldBe` operaGame

    it "is the inverse of toFEN" $
      toFEN operaGame `shouldBe` "rn2kb1r/p3qppp/2p2n2/1N2p1B1/2B1P3/1Q6/PPP2PPP/R3K2R b KQkq - 0 10"
