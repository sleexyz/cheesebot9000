{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module LegalMoves where

import Chess.Internal.Board
import Chess.Internal.Move
import Chess.Internal.Piece
import Chess.Internal.Game
import Chess as C
import Chess.FEN

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Random

-- for the ngw server-specific stuff:
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HMap
import qualified Data.Text as T
import Network.HTTP






allLegalMoves :: GameState -> [Move]
allLegalMoves state =
   let allOfEm :: [Move]
       allOfEm = generateAllPotentialMoves state
   in filter ((\case Right _ -> True ; _ -> False) . applyMove state) allOfEm



