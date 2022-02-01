module Test.Util where

import           Chess.Base

import           Control.Monad

import           Test.QuickCheck

coords :: Gen Coordinate
coords = choose ('a', 'h') >>= (\x -> liftM (Coordinate x) (choose (1, 8)))
