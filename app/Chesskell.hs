{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Aeson.Types

import Chess.AI
import Chess.Base
import Chess.Bitboard
import qualified Chess.FenParser as FPOld
import Chess.FastFenParser
import Chess.Game
import Chess.MoveGen
import Chess.Predicates

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Happstack.Server
import Happstack.Server.Types

import System.Environment
import System.IO

import Text.Parsec

data GameResult = InProgress | WhiteWin | BlackWin | Stalemate

data JsonMove = JsonMove
  { fenString :: String
  , jsonMoveFrom  :: String
  , jsonMoveTo    :: String
  , jsonMoveSuccessful :: Bool
  , jsonMoveGameResult :: GameResult
  }

instance ToJSON JsonMove where
  toJSON (JsonMove fen from to successful result) = object ["fen" .= fen, "from" .= from, "to" .= to, "successful" .= successful, "result" .= result]

instance ToJSON GameResult where
  toJSON InProgress = "*"
  toJSON WhiteWin = "1-0"
  toJSON BlackWin = "0-1"
  toJSON Stalemate = "1/2-1/2"

coordPairToMove :: (Game BitboardRepresentation) -> Coordinate -> Coordinate -> [Move]
coordPairToMove game from to = filter (\move -> (moveFrom move == from) && (moveTo move == to)) . pseudoLegalMoves $ game

determineGameResult   :: (Game BitboardRepresentation) -> GameResult
determineGameResult g = case ((isCheckmate g (activeColor g)), (isStalemate g (activeColor g))) of
                          (True, False) -> if (activeColor g == White) then BlackWin else WhiteWin
                          (False, True) -> Stalemate
                          (False, False) -> InProgress

requestMoveHandler :: ServerPart Response
requestMoveHandler = do

  req <- askRq

  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  fenString <- lookBS "fen"

  let position = either (const undefined) id $ fastParseFEN fenString

  let chosenAIMove = decideOnMove (activeColor position) position
  let chosenAIMoveFrom = case (moveFrom chosenAIMove) of
                           Coordinate f r -> return f ++ show r
  let chosenAIMoveTo = case (moveTo chosenAIMove) of
                           Coordinate f r -> return f ++ show r

  let positionAfterAIMove = fromJust $ makeMoveFrom position chosenAIMove

  let gameResult = determineGameResult positionAfterAIMove

  ok (toResponseBS "application/json" $ encode (JsonMove (fastToFEN positionAfterAIMove) chosenAIMoveFrom chosenAIMoveTo True gameResult))

makeMoveHandler :: ServerPart Response
makeMoveHandler = do

  req <- askRq

  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  fenString <- lookBS "fen"
  jsonMoveFrom  <- look "from"
  jsonMoveTo    <- look "to"

  let position = either (const undefined) id $ fastParseFEN fenString

  let fromCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser FPOld.enPassantSquareParser (FPOld.FenParserState $ Coordinate 'a' 8) "" (jsonMoveFrom ++ " ")
  let toCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser FPOld.enPassantSquareParser (FPOld.FenParserState $ Coordinate 'a' 8) "" (jsonMoveTo ++ " ")

  let plyMove = case (coordPairToMove position fromCoord toCoord) of
                  [] -> Nothing
                  xs -> Just $ head (coordPairToMove position fromCoord toCoord)

  case plyMove of
    Nothing -> ok (toResponseBS "application/json" $ encode (JsonMove (fastToFEN position) "" "" False BlackWin))
    Just m -> do
      let plyMoveFrom = case (moveFrom m) of
                               Coordinate f r -> return f ++ show r
      let plyMoveTo = case (moveTo m) of
                               Coordinate f r -> return f ++ show r

      let positionAfterPlayerMove = makeMoveFrom position m

      let response = case positionAfterPlayerMove of
                       Nothing -> ok (toResponseBS "application/json" $ encode (JsonMove (fastToFEN position) plyMoveFrom plyMoveTo False BlackWin))
                       Just newPosition -> ok (toResponseBS "application/json" $ encode (JsonMove (fastToFEN newPosition) plyMoveFrom plyMoveTo True (determineGameResult newPosition)))

      response


main :: IO ()
main = do
  envPort <- getEnv "PORT"
  hSetBuffering stdout NoBuffering
  print $ "Listening on port " ++ envPort
  simpleHTTP (nullConf { port = read envPort, timeout = 300 })
    $ msum [ dir "makemove" $ do method POST
                                 makeMoveHandler
           , dir "requestmove" $ do method POST
                                    requestMoveHandler
           , dir "js" $ serveDirectory DisableBrowsing ["index.html"] "cheskell/js"
           , dir "css" $ serveDirectory DisableBrowsing ["index.html"] "cheskell/css"
           , serveFile (guessContentTypeM mimeTypes) "cheskell/index.html"
           ]
