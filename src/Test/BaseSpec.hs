module BaseSpec where

import Chess.Base

import Control.Monad.State.Lazy

import Test.Hspec
import Test.Placements

kingOpening :: RegularGame
kingOpening = RegularGame
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
  , activeColor     = Black
  , castlingRights  = CastleRights True True True True
  , enPassantSquare = Nothing
  , halfMoveClock   = 0
  , fullMoveNumber  = 1
  }

main :: IO ()
main = hspec $

  describe "makeMove" $ do
    it "accepts standard moves and updates the game's positional state" $
      execState (makeMove $ Move { moveFrom = (Coordinate 'e' 2), moveTo = (Coordinate 'e' 4), moveType = Standard }) startingPos `shouldBe` kingOpening

    it "accepts captures and removes the captured piece from the board" $
      execState (makeMove $ Move { moveFrom = (Coordinate 'd' 5), moveTo = (Coordinate 'c' 4), moveType = Capture })
        (setupGame [ (Piece Pawn White, Coordinate 'c' 4)
                   , (Piece Pawn Black, Coordinate 'd' 5)
                   ]) { activeColor = Black } `shouldBe` (setupGame [ (Piece Pawn Black, Coordinate 'c' 4) ]) { activeColor = White }

    it "accepts white kingside castles, updating castling rights and moving the rook" $
      execState (makeMove $ Move { moveFrom = (Coordinate 'e' 1), moveTo = (Coordinate 'g' 1), moveType = Castle }) whiteKingOOTest `shouldBe`
        (setupGame [ (Piece King White, Coordinate 'g' 1)
                   , (Piece Rook White, Coordinate 'f' 1)
                   ]) { activeColor = Black
                      , castlingRights = CastleRights False True False True
                      }
