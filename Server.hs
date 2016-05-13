-- copied from hylogen

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Server where

import Data.Monoid
import qualified Data.Text as T
import Network.WebSockets

import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad


serveThing :: IO ServerRef
serveThing = do
   theVar <- newEmptyMVar :: IO (MVar String)
   t <- forkIO $
      runServer "127.0.0.1" 8080 $ handleConnection theVar
   return $ ServerRef t theVar

handleConnection :: MVar String -> PendingConnection -> IO ()
handleConnection theVar pending = do
   connection <- acceptRequest pending
   forever $ do
      (sendTextData connection . T.pack) =<< takeMVar theVar


data ServerRef = ServerRef ThreadId (MVar String)
 -- deriving (Show, Read, Eq, Ord)

killServer :: ServerRef -> IO ()
killServer (ServerRef t _) = killThread t

putOnServer :: ServerRef -> String -> IO ()
putOnServer (ServerRef _ mv) s = putMVar mv s
