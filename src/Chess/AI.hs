{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess.AI where

import           Chess.Base
import           Chess.Bitboard
import           Chess.Game
import           Chess.MoveGen
import           Chess.Predicates

import           Control.Applicative

import           Data.Bits
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector as V

data GameTree a = GameTree
  { gameTreeValue    :: a
  , gameTreeChildren :: [GameTree a]
  , gameTreeLastMove :: [Move]
  }
  deriving Show

instance Functor GameTree where
  fmap f (GameTree v children moves) =
    GameTree (f v) ((fmap . fmap) f children) moves

instance Ord (Game BitboardRepresentation) where
  compare = comparing evalGame

whitePawnPieceSquareTable :: V.Vector Int
whitePawnPieceSquareTable = V.fromList
  [ 0, 0, 0, 0, 0, 0, 0, 0, 50, 50, 50, 50, 50, 50, 50, 50, 10, 10, 20, 30, 30, 20, 10, 10, 5, 5, 10, 27, 27, 10, 5, 5, 0, 0, 0, 25, 25, 0, 0, 0, 5, -5, -10, 0, 0, -10, -5, 5, 5, 10, 10, -25, -25, 10, 10, 5, 0, 0, 0, 0, 0, 0, 0, 0]

blackPawnPieceSquareTable :: V.Vector Int
blackPawnPieceSquareTable = V.fromList
  [ 0, 0, 0, 0, 0, 0, 0, 0, -5, -10, -10, 25, 25, -10, -10, -5, -5, 5, 10, 0, 0, 10, 5, -5, 0, 0, 0, -25, -25, 0, 0, 0, -5, -5, -10, -27, -27, -10, -5, -5, -10, -10, -20, -30, -30, -20, -10, -10, -50, -50, -50, -50, -50, -50, -50, -50, 0, 0, 0, 0, 0, 0, 0, 0]

evalGame :: (Game BitboardRepresentation) -> Int
evalGame game
  | isCheckmate game (activeColor game)
  = checkmateValue
  | isStalemate game (activeColor game)
  = 0
  | otherwise
  = evalPosition $ placement game
 where

  checkmateValue | activeColor game == White = minBound
                 | otherwise                 = maxBound

-- move into bitboard module
evalPosition :: BitboardRepresentation -> Int
evalPosition BitboardRepresentation
  { whitePawns = wp@(Bitboard wpbits)
  , blackPawns = bp@(Bitboard bpbits)
  , whiteBishops = wb@(Bitboard wbbits)
  , blackBishops = bb@(Bitboard bbbits)
  , whiteKnights = wn@(Bitboard wnbits)
  , blackKnights = bn@(Bitboard bnbits)
  , whiteRooks = wr@(Bitboard wrbits)
  , blackRooks = br@(Bitboard brbits)
  , whiteQueens = wq@(Bitboard wqbits)
  , blackQueens = bq@(Bitboard bqbits)
  } = whitePawnsValue
      + blackPawnsValue
      + whiteBishopsValue
      + blackBishopsValue
      + whiteKnightsValue
      + blackKnightsValue
      + whiteRooksValue
      + blackRooskValue
      + whiteQueensValue
      + blackQueensValue where

  whitePawnsValue :: Int
  whitePawnsValue = foldr ((+) . whitePawnLocValue) ((popCount wpbits) * 100) $ bitboardToSquareIndices wp
  blackPawnsValue :: Int
  blackPawnsValue = foldr ((+) . blackPawnLocValue) ((popCount bpbits) * (-100)) $ bitboardToSquareIndices bp
  whiteBishopsValue :: Int
  whiteBishopsValue = popCount wbbits * 300
  blackBishopsValue :: Int
  blackBishopsValue = popCount bbbits * (-300)
  whiteKnightsValue :: Int
  whiteKnightsValue = popCount wnbits * 300
  blackKnightsValue :: Int
  blackKnightsValue = popCount bnbits * (-300)
  whiteRooksValue :: Int
  whiteRooksValue = popCount wrbits * 500
  blackRooskValue :: Int
  blackRooskValue = popCount brbits * (-500)
  whiteQueensValue :: Int
  whiteQueensValue = popCount wqbits * 900
  blackQueensValue :: Int
  blackQueensValue = popCount bqbits * (-900)

  whitePawnLocValue :: Int -> Int
  whitePawnLocValue = (whitePawnPieceSquareTable V.!)

  blackPawnLocValue :: Int -> Int
  blackPawnLocValue = (blackPawnPieceSquareTable V.!)

-- Inspired by John Hughes' "Why Functional Programming Matters"

gamesFrom :: (Game BitboardRepresentation) -> [(Move, (Game BitboardRepresentation))]
gamesFrom game = mapMaybe (\move -> (move, ) <$> makeMoveFrom game move)
                          (pseudoLegalMoves game)

gameTreeFrom
  :: ((Game BitboardRepresentation) -> [(Game BitboardRepresentation)])
  -> [Move]
  -> (Game BitboardRepresentation)
  -> GameTree (Game BitboardRepresentation)
gameTreeFrom f moves game = GameTree
  game
  (map (\edge -> (gameTreeFrom f ((fst edge) : moves) (snd edge)))
       (gamesFrom game)
  )
  moves

maxByGameValue :: [GameTree (Game BitboardRepresentation)] -> GameTree (Game BitboardRepresentation)
maxByGameValue = maximumBy (comparing gameTreeValue)

minByGameValue :: [GameTree (Game BitboardRepresentation)] -> GameTree (Game BitboardRepresentation)
minByGameValue = minimumBy (comparing gameTreeValue)

minimize :: GameTree (Game BitboardRepresentation) -> GameTree (Game BitboardRepresentation)
minimize = minByGameValue . minimize'

minimize' :: GameTree (Game BitboardRepresentation) -> [GameTree (Game BitboardRepresentation)]
minimize' (GameTree v []       moves) = [GameTree v [] moves]
minimize' (GameTree _ children _    ) = mapMax (map maximize' children) where

  mapMax []       = []
  mapMax (x : xs) = (maxByGameValue x) : (omit (maxByGameValue x) xs)

  omit pot [] = []
  omit pot (x : xs)
    | mingeq x pot = omit pot xs
    | otherwise    = (maxByGameValue x) : (omit (maxByGameValue x) xs)

  mingeq [] pot = False
  mingeq (x : xs) pot
    | (evalGame $ gameTreeValue x) >= (evalGame $ gameTreeValue pot) = True
    | otherwise = mingeq xs pot

maximize :: GameTree (Game BitboardRepresentation) -> GameTree (Game BitboardRepresentation)
maximize = maxByGameValue . maximize'

maximize' :: GameTree (Game BitboardRepresentation) -> [GameTree (Game BitboardRepresentation)]
maximize' (GameTree v []       moves) = [GameTree v [] moves]
maximize' (GameTree _ children _    ) = mapMin (map minimize' children) where

  mapMin []       = []
  mapMin (x : xs) = (minByGameValue x) : (omit (minByGameValue x) xs)

  omit pot [] = []
  omit pot (x : xs)
    | minleq x pot = omit pot xs
    | otherwise    = (minByGameValue x) : (omit (minByGameValue x) xs)

  minleq [] pot = False
  minleq (x : xs) pot
    | (evalGame $ gameTreeValue x) <= (evalGame $ gameTreeValue pot) = True
    | otherwise = minleq xs pot

prune :: Int -> GameTree a -> GameTree a
prune 0 (GameTree v _ moves) = GameTree v [] moves
prune n (GameTree v children moves) =
  GameTree v (map (prune $ n - 1) children) moves

decideOnMove :: Player -> (Game BitboardRepresentation) -> Move
decideOnMove player game
  | player == White
  = head $ reverse $ gameTreeLastMove $ maximize $ prune 4 $ gameTreeFrom
    (mapMaybe (makeMoveFrom game) . pseudoLegalMoves)
    []
    game
  | otherwise
  = head $ reverse $ gameTreeLastMove $ minimize $ prune 4 $ gameTreeFrom
    (mapMaybe (makeMoveFrom game) . pseudoLegalMoves)
    []
    game
