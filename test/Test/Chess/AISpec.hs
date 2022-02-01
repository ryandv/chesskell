module Test.Chess.AISpec where

import Chess.AI
import Chess.Base
import Chess.Bitboard
import Chess.FenParser
import Chess.Game
import Chess.MoveGen

import Control.Monad
import Control.Monad.State.Lazy

import Test.Placements

import Test.Hspec
import Test.QuickCheck

successful (Right x) = x

liftState :: (Monad m) => Control.Monad.State.Lazy.State s a -> StateT s m a
liftState action = StateT $ \s -> return (runState action s)

makeTwoMoves :: StateT ChessGame IO (Move, Move)
makeTwoMoves = do
  curState <- get
  let gamestate = gameState curState

  let chosenMove = decideOnMove (activeColor gamestate) gamestate

  liftState (makeMove chosenMove)

  stateAfterAIMove <- get

  liftState (makeMove $ (Capture (Coordinate 'g' 7) (Coordinate 'h' 6)))

  stateAfterPlayerMove <- get
  let gameStateAfterPlayerMove = gameState stateAfterPlayerMove

  let chosenMove2 = decideOnMove (activeColor gameStateAfterPlayerMove) gameStateAfterPlayerMove

  liftState (makeMove chosenMove2)

  return (chosenMove, chosenMove2)


spec :: Spec
spec =
  context "finding checkmate" $ do

    describe "Mate in 2" $ do

      it "can find mate in 2" $ do
        let position = (successful $ parseFen "" "r6k/pp1b2p1/3Np2p/8/3p1PRQ/2nB4/q1P4P/2K5 w - - 0 1")
        evalStateT makeTwoMoves (ChessGame position (regularGameToBitboardGame position)) `shouldReturn`
          ( (Capture (Coordinate 'h' 4) (Coordinate 'h' 6))
          , (Move (Coordinate 'd' 6) (Coordinate 'f' 7))
          )
