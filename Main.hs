-- we use .Internal, so have to do -i/home/air/box_build/hchesslib/src
-- there's also e.g. 'generateAllRookMoves'


-- ghc -prof -i/home/air/box_build/hchesslib/src -auto-all -O2 -static -rtsopts -threaded Main.hs


-- TODO: the biggest thing i'm not building into this atm is the notion
--   of computing while waiting for the other player's move
  -- AND SAVING THE COMPUTATION WE'VE ALREADY DONE!

-- idea is, take slightly less time than the opponent did last time
-- or easier to do (but less good): take 5 secs per move: assume max 60 moves per game


    -- iterative deepening
     -- search depth of 1, 2, 3, etc as time permits
     -- 'take 10' can then save the 'losers' to be computed in extra time




-- TODODODODODODODODOD
-- 'generateAllPotentialMoves' is BROKEN -- use 'allLegalMoves' everywhere instead -- grep for it


-- todo: just hardcocde the very first opener!



-- TO RUN:
-- call main or 'pitAgainst' or w/e

   --  s <- serveThing
   --  pitAgainstVisual s idiot idiot
   --  killServer s

-- then point a browser to localhost:8080



{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

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
import System.Environment (getArgs)

import Players
import Server

{-
main = do
    s <- serveThing 
    -- pitAgainstVisual s minnieMouse idiot
    -- pitAgainstVisual s minnieMouse minnieMouse
    -- pitAgainstVisual s idiot minnieMouse
    -- pitAgainstVisual s pruno idiot -- minnieMouse
    -- pitAgainstVisual s prius idiot -- minnieMouse
    -- pitAgainstVisual s prius idiot -- minnieMouse
    -- pitAgainstVisual s prius quasi
    -- pitAgainstVisual s prius bestie
    pitAgainstVisual s bestie bestie
    return ()
-}

-- main = playerOnMax'sServer  bestie White "http://52.200.188.234:3000/robot/test/88"

main = getArgs >>= \case
   [color, playerName, gameNum] ->
      playerOnMax'sServer  bestie (readCol color) $ "http://52.200.188.234:3000/robot/" ++ playerName ++ "/" ++ gameNum
   _ -> error "args: color playerName game_num"
 where
   -- TODO: can read the color from the original game JSON
   readCol "black" = Black
   readCol "white" = White

-- main = pitAgainstN minnieMouse idiot 10
-- main = pitAgainstN pruno idiot 1

main' = do
   fooVar <- serveThing

   let allOfEm = generateAllPotentialMoves initialState

   putOnServer fooVar $ writeFEN $ (\(Right x)->x) $ applyMove initialState (head allOfEm)
   print allOfEm
   -- "The only supported move format at the moment is coordinate notation."
   -- move
   print $ applyMove initialState (head allOfEm)
   -- print $ writeFEN (head allOfEm)
   print $ writeFEN $ (\(Right x)->x) $ applyMove initialState (head allOfEm)

   -- allFirstMoves allOfEm fooVar

   -- playRandomMove fooVar initialState

   -- pitAgainstVisual fooVar idiot idiot

   print $ length $ concatMap (\(Right state) -> generateAllPotentialMoves state) $ map (applyMove initialState) $ generateAllPotentialMoves initialState

allFirstMoves allOfEm fooVar =
   forM_ allOfEm $ \move -> do
      putStrLn "posting"
      putOnServer fooVar $ writeFEN $ (\(Right x)->x) $
         applyMove initialState move
      putStrLn "posted"
      threadDelay (1 * (10^5))



type Player = Color -> GameState -> IO Move

-- | 2nd arg is white player, 3rd is black player
pitAgainstVisual :: ServerRef -> Player -> Player -> IO ()
pitAgainstVisual positionVar whitePlayer blackPlayer = do
   foo initialState
 where
   foo :: GameState -> IO () -- GameState
   foo state = do
      m <- getMove state whitePlayer blackPlayer
      let newState = case applyMove state m of
            Right ns -> ns
            Left _ -> error $ "bad move!: " ++ show m
      putOnServer positionVar $ writeFEN newState
      threadDelay (10^5)
      foo newState

{-
   where
      moves = cycle [whitePlayer, blackPlayer] -- nvmd, what about pawns changing at end of board? that's 2 moves in a row by the same player i think
-}

getMove :: GameState -> Player -> Player -> IO Move
getMove state whitePlayer blackPlayer =
      ($ state) $ case currentPlayer state of
         White -> whitePlayer White
         Black -> blackPlayer Black

data GameResult
   = WhiteWins
   | BlackWins
   | Stalemate
 deriving (Show, Read, Eq, Ord)

pitAgainst :: Player -> Player -> IO GameResult -- (Maybe (Either () ()))
pitAgainst whitePlayer blackPlayer =
   pitAgainst' initialState 20
 where
   pitAgainst' :: GameState -> Int -> IO GameResult
   pitAgainst' _ 0 = pure Stalemate -- TEMP!!!!!
   pitAgainst' oldState n = do
      move <- getMove oldState whitePlayer blackPlayer
      let Right newState = applyMove oldState move
      if C.isDraw newState
         then pure Stalemate
         else case winner newState of
            Just White -> pure WhiteWins
            Just Black -> pure BlackWins
            Nothing -> pitAgainst' newState (n-1)

pitAgainstN :: Player -> Player -> Int -> IO [(GameResult, Int)]
pitAgainstN whitePlayer blackPlayer numTimes = do
   results <- replicateM numTimes $ pitAgainst whitePlayer blackPlayer
   pure $ Map.toList $ Map.fromListWith (+) $ map (,1) results


-- oh man... those are some pretty shitty random moves...
idiot :: {- MVar String -> -} Color -> GameState -> IO Move
idiot _ startState = do
   (newMove, _newPos) <- getNews
   -- 'generateAllPotential' doesn't account for if you're in check
   return newMove
{-
   (putMVar theVar . writeFEN) newPos
   threadDelay (10^5)
   idiot theVar newPos
-}
 where
   -- this is a hack cause 'generateAllPossible' doesn't rule out in-check-ness
   -- getNews ::
   getNews = do
      newMove <- pick (generateAllPotentialMoves startState)
      case applyMove startState newMove of
         Right x -> pure (newMove, x)
         Left y -> do
            -- putStrLn $ "illegal: " ++ show y ++ show newMove
            getNews



getTest = getShit "http://52.200.188.234:3000/test/2"

getShit :: String -> IO (Double, Double, GameState)
getShit url = do
   response <- getResponseBody =<< simpleHTTP (getRequest url)
   putStrLn response
   let obj = case decode (BSL8.pack response) :: Maybe Value of
          Just (Object o) -> o
          _ -> error $ "shit couldnt decode r8fhfw  " ++ show response
   -- forM_ (HMap.toList obj) print
   let board = case obj HMap.! "fen" of
          String ((readFEN . T.unpack) -> Just b) -> b
          _ -> error $ "shit couldnt decode 234f " ++ show response
   return (undefined, undefined, board)

pick :: MonadRandom m => [a] -> m a
pick l = (l !!) <$> getRandomR (0, (length::[a]->Int) l - 1)




-- player :: GameState -> IO GameState
{-
idiot :: GameState -> IO GameState
idiot state =
-}

printMove :: Move -> String
printMove = \case
   Movement _Piece fromCoord toCoord ->
      printCoordinate fromCoord ++ {- "-"++ -} printCoordinate toCoord
   Capture _Piece fromCoord toCoord ->
      printCoordinate fromCoord ++ {- "-"++ -} printCoordinate toCoord
   EnPassant _ from to ->
      printCoordinate from ++ {- "-"++ -} printCoordinate to
   PawnDoubleMove _ from to ->
      printCoordinate from ++ printCoordinate to
-- NOTE THESE 2 ARE IMPLEMENTATION SPECIFIC:
   Castling color castleType ->
      case (color, castleType) of
         (White, Short) -> "e1g1"
         (White, Long) -> "e1b1"
         (Black, Short) -> "e8g8"
         (Black, Long) -> "e8b8"
   Promotion _ from to pieceType -> -- just assumes it's always queen on max's
      printCoordinate from ++ printCoordinate to


postMove :: String -> Move -> IO ()
postMove url move = do
   let foo = url ++ "?move=" ++ printMove move
   print $ ("posting", foo)
   resp <- simpleHTTP $ postRequest $ foo
   print =<< (fmap (take 100) $ getResponseBody resp)
   return ()

-- type Player = Color -> GameState -> IO Move
playerOnMax'sServer :: Player -> Color -> String -> IO ()
playerOnMax'sServer player ourColor url =
   forever $ do
      (_, _, currentState) <- getShit url
      when (currentPlayer currentState == ourColor) $ do
         newMove <- player ourColor currentState
         putStrLn  $ "move" ++ show newMove
         postMove url newMove
      threadDelay (10^5)

