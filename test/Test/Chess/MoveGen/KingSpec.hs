module Test.Chess.MoveGen.KingSpec where

import Chess.Base
import Chess.Bitboard
import Chess.FenParser
import Chess.Game

import Chess.MoveGen
import Chess.MoveGen.King

import Control.Monad
import Control.Monad.State.Lazy

import Test.Placements
import Test.Placements.King

import Test.Hspec
import Test.QuickCheck
import Test.Util

spec :: Spec
spec = describe "potentialKingMoves" $
         context "movement" $ do
           -- TODO: add quickcheck spec for when CastleRights are True
           it "never produces moves off the board" $
             property $ forAll coords $ \c -> all (isOnBoard . moveTo) $ potentialKingMoves
               (CastleRights False False False False)
               (regularToBitboard . placement $ (placePiece emptyTest (Piece King White) c))
               White
               c

           it "allows the king to move to any square in its Moore neighbourhood" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ onlyKingTest) White (Coordinate 'd' 4) `shouldMatchList`
               [ Move (Coordinate 'd' 4) (Coordinate 'c' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 3)
               ]

           it "allows the white king to castle kingside, if he has the right to" $
             potentialKingMoves (CastleRights True False False False) (regularToBitboard . placement $ whiteKingOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               , Castle (Coordinate 'e' 1) (Coordinate 'g' 1)
               ]

           it "does not allow the white king to castle kingside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ whiteKingOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               ]

           it "allows the white king to castle queenside, if he has the right to" $
             potentialKingMoves (CastleRights False False True False) (regularToBitboard . placement $ whiteKingOOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               , Castle (Coordinate 'e' 1) (Coordinate 'c' 1)
               ]

           it "does not allow the white king to castle queenside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ whiteKingOOOTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               ]

           it "allows the white king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves (CastleRights True False True False) (regularToBitboard . placement $ whiteKingBothCastlesTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               , Castle (Coordinate 'e' 1) (Coordinate 'g' 1)
               , Castle (Coordinate 'e' 1) (Coordinate 'c' 1)
               ]

           it "does not allow the white king to castle when he is not on his home square" $
             potentialKingMoves (CastleRights True True True True) (regularToBitboard . placement $ whiteKingMovedNoOOTest) White (Coordinate 'd' 4) `shouldMatchList`
               [ Move (Coordinate 'd' 4) (Coordinate 'c' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 3)
               ]

           it "does not allow the white king to castle kingside when the rook is not on its home square" $
             potentialKingMoves (CastleRights True False False False) (regularToBitboard . placement $ whiteKingNoRookCastleTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               ]

           it "does not allow the white king to castle queenside when the rook is not on its home square" $
             potentialKingMoves (CastleRights False False True False) (regularToBitboard . placement $ whiteKingNoRookCastleTest) White (Coordinate 'e' 1) `shouldMatchList`
               [ Move (Coordinate 'e' 1) (Coordinate 'd' 1)
               , Move (Coordinate 'e' 1) (Coordinate 'd' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'e' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 2)
               , Move (Coordinate 'e' 1) (Coordinate 'f' 1)
               ]

           it "allows the black king to castle kingside, if he has the right to" $
             potentialKingMoves (CastleRights False True False False) (regularToBitboard . placement $ blackKingOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               , Castle (Coordinate 'e' 8) (Coordinate 'g' 8)
               ]

           it "does not allow the black king to castle kingside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ blackKingOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               ]

           it "allows the black king to castle queenside, if he has the right to" $
             potentialKingMoves (CastleRights False False False True) (regularToBitboard . placement $ blackKingOOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               , Castle (Coordinate 'e' 8) (Coordinate 'c' 8)
               ]

           it "does not allow the black king to castle queenside, if he does not have the right to" $
             potentialKingMoves (CastleRights False False False False) (regularToBitboard . placement $ blackKingOOOTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               ]

           it "allows the black king to castle both kingside or queenside, if he has the option" $
             potentialKingMoves (CastleRights False True False True) (regularToBitboard . placement $ blackKingBothCastlesTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               , Castle (Coordinate 'e' 8) (Coordinate 'g' 8)
               , Castle (Coordinate 'e' 8) (Coordinate 'c' 8)
               ]

           it "does not allow the black king to castle when he is not on his home square" $
             potentialKingMoves (CastleRights True True True True) (regularToBitboard . placement $ blackKingMovedNoOOTest) Black (Coordinate 'd' 4) `shouldMatchList`
               [ Move (Coordinate 'd' 4) (Coordinate 'c' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 5)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 4)
               , Move (Coordinate 'd' 4) (Coordinate 'e' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'd' 3)
               , Move (Coordinate 'd' 4) (Coordinate 'c' 3)
               ]

           it "does not allow the black king to castle kingside when the rook is not on its home square" $
             potentialKingMoves (CastleRights False True False False) (placement . regularGameToBitboardGame $ blackKingNoRookCastleTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               ]

           it "does not allow the black king to castle queenside when the rook is not on its home square" $
             potentialKingMoves (CastleRights False False False True) (placement . regularGameToBitboardGame $ blackKingNoRookCastleTest) Black (Coordinate 'e' 8) `shouldMatchList`
               [ Move (Coordinate 'e' 8) (Coordinate 'd' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 8)
               , Move (Coordinate 'e' 8) (Coordinate 'f' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'e' 7)
               , Move (Coordinate 'e' 8) (Coordinate 'd' 7)
               ]

           it "does not allow the king to castle if the intermediate squares are occupied" $
             potentialKingMoves (CastleRights True False False False) (placement startingPos) White (Coordinate 'e' 1) `shouldMatchList`
               []

           it "does not generate castling moves for the opposing colour" $ do
             let successful (Right x) = x
             let position = successful $ parseFen "" "r2q1k1r/pp3ppp/4b3/2pN4/4P3/4N3/PP1Q1PPP/R3K2R w - - 0 1"
             potentialKingMoves (CastleRights True True False False) (placement $ regularGameToBitboardGame position) Black (Coordinate 'e' 1) `shouldNotContain`
               [ Castle (Coordinate 'e' 1) (Coordinate 'g' 1) ]
