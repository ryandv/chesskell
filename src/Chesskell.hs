{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson.Encode
import Data.Aeson.Types

import Chess.AI
import Chess.Base
import Chess.FenParser
import Chess.Game
import Chess.MoveGen

import Control.Monad
import Control.Monad.Trans

import Data.Maybe

import Happstack.Server
import Happstack.Server.Types

import System.Environment

import Text.Parsec

data JsonMove = JsonMove
  { fenString :: String
  , jsonMoveFrom  :: String
  , jsonMoveTo    :: String
  }

instance ToJSON JsonMove where
  toJSON (JsonMove fen from to) = object ["fen" .= fen, "from" .= from, "to" .= to]

coordPairToMove :: RegularGame -> Coordinate -> Coordinate -> [Move]
coordPairToMove game from to = filter (\move -> (moveFrom move == from) && (moveTo move == to)) $ pseudoLegalMoves game

makeMoveHandler :: ServerPart Response
makeMoveHandler = do

  req <- askRq

  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  fenString <- look "fen"
  jsonMoveFrom  <- look "from"
  jsonMoveTo    <- look "to"

  let position = either (const undefined) id $ parseFen "" fenString

  let fromCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser enPassantSquareParser (FenParserState $ Coordinate 'a' 8) "" (jsonMoveFrom ++ " ")
  let toCoord = either (const $ Coordinate 'a' 1) fromJust $ runParser enPassantSquareParser (FenParserState $ Coordinate 'a' 8) "" (jsonMoveTo ++ " ")

  let move = head (coordPairToMove position fromCoord toCoord)

  let positionAfterPlayerMove = fromJust $ makeMoveFrom position move

  let chosenAIMove = decideOnMove (activeColor positionAfterPlayerMove) positionAfterPlayerMove
  let chosenAIMoveFrom = case (moveFrom chosenAIMove) of
                           Coordinate f r -> return f ++ show r
  let chosenAIMoveTo = case (moveTo chosenAIMove) of
                           Coordinate f r -> return f ++ show r

  let positionAfterAIMove = fromJust $ makeMoveFrom positionAfterPlayerMove chosenAIMove

  ok (toResponse $ encode (JsonMove (toFEN positionAfterAIMove) chosenAIMoveFrom chosenAIMoveTo))


main :: IO ()
main = do
  envPort <- getEnv "PORT"
  simpleHTTP (nullConf { port = read envPort })
    $ msum [ dir "makemove" $ do method POST
                                 makeMoveHandler
           , dir "js" $ serveDirectory DisableBrowsing ["index.html"] "/tmp/cheskell/js"
           , dir "css" $ serveDirectory DisableBrowsing ["index.html"] "/tmp/cheskell/css"
           , serveFile (guessContentTypeM mimeTypes) "/tmp/cheskell/index.html"
           ]
