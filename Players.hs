{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Players where

import Chess.Internal.Board
import Chess.Internal.Move
import Chess.Internal.Piece
import Chess.Internal.Game
import Chess as C
import Chess.FEN

import Control.Arrow
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Ord
import System.Random.Shuffle

import LegalMoves

-- add bang for speed:
data Tree a = Tree a [Tree a]
 deriving (Show, Eq) -- , Functor)

instance Functor Tree where
   fmap f (Tree x xs) = Tree (f x) $ map (fmap f) xs

data Score
   -- = PositiveInfinity
   = Finite {-# UNPACK #-} !Float -- only this, so we can multiply by 'infinity' for front-weighting (having them closer to the present)
   -- | NegativeInfinity
 deriving (Show, Eq)

instance Ord Score where
   -- most common case goes first:
   Finite a <= Finite b = a <= b
{-
   NegativeInfinity <= _ = True
   PositiveInfinity <= PositiveInfinity = True
   PositiveInfinity <= _ = False
   Finite _ <= PositiveInfinity = True
   Finite _ <= NegativeInfinity = False
-}

-- | Basic bounded minimax
minnieMouse :: Color -> GameState -> IO Move
minnieMouse = -- myColor state =
   shit (pure . (\t -> chopTree t 2)) minimax

trimNum = 8

shit :: (Tree GameState -> IO (Tree GameState)) -> (Tree Score -> Score) -> Color -> GameState -> IO Move
shit trimmer strategy myColor state = do
   x_untrimmed <- forM (allMovesFrom state) $ \(m, t) -> do
      trimmed <- trimmer t
      pure (m, trimmed)
   x <- take trimNum <$> shuffleM x_untrimmed
   let allMoves :: [(Move, Score)] -- Tree Score)]
         -- this doesn't seem to do much good...:
       allMoves = parMap rpar {- rseq -} (second strategy . second (scoreTree myColor)) $ x
       bestMove = (L.maximumBy (comparing snd)::[(a,Score)]->(a,Score)) $ allMoves
   print bestMove >> pure (fst bestMove)

minimax :: Tree Score -> Score
minimax = minimax' Us
 where
   minimax' _ (Tree x []) = x
   minimax' Us (Tree x xs) = minimum $ minimax' Them <$> xs
   minimax' Them (Tree x xs) = maximum $ minimax' Us <$> xs

-- | amtToFrontWeight
-- 1 is minimax, 0, greedy af
minimaxFrontWeighted :: Float -> Tree Score -> Score
minimaxFrontWeighted amtToFrontWeight = minimax' Us
 where
   minimax' _ (Tree x []) = x
   minimax' Us (Tree x xs) = mix amtToFrontWeight
      (minimum $ minimax' Them <$> xs)
      (minimum $ map (\(Tree v _)->v) xs)
   minimax' Them (Tree x xs) = mix amtToFrontWeight
      (maximum $ minimax' Us <$> xs)
      (maximum $ map (\(Tree v _)->v) xs)

mix n (Finite x) (Finite y) = Finite $ x * n + y * (1 - n)



data UsOrThem = Us | Them


alphaBetaFrontWeighted :: Float -> Tree Score -> Score
alphaBetaFrontWeighted amtToWeight t = alphaBeta' Us (Finite (-10000), Finite 10000) t -- (NegativeInfinity, PositiveInfinity) t
 where
   alphaBeta' :: UsOrThem -> (Score, Score) -> Tree Score -> Score
   alphaBeta' _ _ (Tree x []) = x
   alphaBeta' color (alpha, beta) (Tree x xs) = fn color $ L.foldl' (sub color) (alpha, beta) xs
    where
      fn :: UsOrThem -> (Score, Score) -> Score
      -- fn Them = fst
      -- fn Us = snd
      fn Us tup = mix (amtToWeight) (snd tup) (maximum $ map (\(Tree x _)->x) xs)
      fn Them tup = mix amtToWeight (fst tup) (minimum $ map (\(Tree x _)->x) xs)

      sub :: UsOrThem -> (Score, Score) -> Tree Score -> (Score, Score)
      sub Them (alpha, beta) n
         | alpha >= beta = (alpha, beta)
         | otherwise = (alpha `max` alphaBeta' Us (alpha, beta) n, beta)
      sub Us (alpha, beta) n
         | alpha >= beta = (alpha, beta)
         | otherwise = (alpha, beta `min` alphaBeta' Them (alpha, beta) n)



alphaBeta :: Tree Score -> Score
alphaBeta t = alphaBeta' Us (Finite (-10000), Finite 10000) t -- (NegativeInfinity, PositiveInfinity) t
 where
   alphaBeta' :: UsOrThem -> (Score, Score) -> Tree Score -> Score
   alphaBeta' _ _ (Tree x []) = x
   alphaBeta' color (alpha, beta) (Tree x xs) = fn color $ L.foldl' (sub color) (alpha, beta) xs
    where
      fn :: UsOrThem -> (Score, Score) -> Score
      fn Them = fst
      fn Us = snd

      sub :: UsOrThem -> (Score, Score) -> Tree Score -> (Score, Score)
      sub Them (alpha, beta) n
         | alpha >= beta = (alpha, beta)
         | otherwise = (alpha `max` alphaBeta' Us (alpha, beta) n, beta)
      sub Us (alpha, beta) n
         | alpha >= beta = (alpha, beta)
         | otherwise = (alpha, beta `min` alphaBeta' Them (alpha, beta) n)

-- pruno :: Color -> GameState -> IO Move
-- | Basic alpha-beta minimax tree search thing
pruno = -- myColor state =
   shit (pure . (`chopTree` 2))  alphaBeta

-- | Hybrid pseudo-monte-carlo alpha-beta search
prius = shit (only10Moves . (`chopTree` 3)) alphaBeta

quasi = shit (only10Moves . (`chopTree` 3)) (minimaxFrontWeighted 0.75)

-- madMax = shit (only10Moves . (`chopTree` 3)) (alphaBetaFrontWeighted 0.9)
bestie = shit (only10Moves . (`chopTree` 4)) (alphaBetaFrontWeighted 0.7) -- 0.9)

-- TEMP! only 7!!:
   -- no wait not 'trimNum'
only10Moves :: Tree GameState -> IO (Tree GameState)
only10Moves (Tree x allStates) = do
   lucky10States <- take trimNum <$> shuffleM allStates
   Tree x <$> mapM only10Moves lucky10States

allPiecesOnBoard :: GameState -> [Piece]
allPiecesOnBoard state =
   (flip mapMaybe) (A.elems $ stateBoard state) $ \case
      Empty -> Nothing
      Square piece -> Just piece

minniePieceScoreFunction :: PieceType -> Float
minniePieceScoreFunction = \case
   Pawn -> 1
   Knight -> 3
   Bishop -> 3
   Rook -> 5
   Queen -> 9
   King -> 0

instance Ord Color where
   White <= Black = True
   Black <= White = False
   _ <= _ = True


evalScoreFunction :: (PieceType -> Float) -> GameState -> Map Color Score
evalScoreFunction evalF gameState =
   let theFinites =
         Map.fromListWith (+) $ (flip map) (allPiecesOnBoard $ gameState) $ \(Piece color p) ->
           (color, evalF p)
   in if C.isDraw gameState
         then Map.fromList [(White, Finite 0),(Black,Finite 0)]
         else case winner gameState of
            Just White -> Map.fromList [(White, Finite 10000),(Black,Finite (-10000))] -- TODO
            Just Black -> Map.fromList [(White, Finite (-10000)),(Black,Finite 10000)] -- TODO
            -- Just White -> Map.fromList [(White, PositiveInfinity),(Black,NegativeInfinity)] -- TODO
            -- Just Black -> Map.fromList [(White, NegativeInfinity),(Black,PositiveInfinity)] -- TODO
            Nothing -> Map.map Finite theFinites
 
-- | Positive is better for white
reinfeld :: Color -> Map Color Score -> Score
reinfeld myColor m = -- (m Map.! White) - (m Map.! Black)
   case (m Map.! myColor, m Map.! oppositeColor myColor) of
      -- (PositiveInfinity, NegativeInfinity) -> PositiveInfinity
      -- (NegativeInfinity, PositiveInfinity) -> NegativeInfinity
      (Finite x, Finite y) -> Finite $ x - y

oppositeColor :: Color -> Color
oppositeColor White = Black
oppositeColor Black = White

-- | All moves in all of chess!
allMovesFrom :: GameState -> {- Int -> -} [(Move, Tree GameState)] -- [Tree (Move, GameState)]
allMovesFrom startState = -- numDeepToGo
   zipWith (,) (allLegalMoves startState) $
      allMovesFrom' startState

allMovesFrom' :: GameState -> [Tree GameState]
allMovesFrom' startState =
   {- Tree startState $ -} mapMaybe foo (allLegalMoves startState)
 where
   foo :: Move -> {- (Move, -} Maybe (Tree GameState) -- ) -- Tree (Move, GameState)
   foo move = case applyMove startState move of
      Right nextState -> Just $ Tree ({- move, -} nextState) $ allMovesFrom' nextState
      Left _ -> error "impossible!" -- Nothing
      -- Right x -> Tree x $ allMovesFrom x 

scoreTree :: Color -> Tree ({- Move, -} GameState) -> Tree ({- Move, -} Score)
scoreTree myColor = fmap (\( s) -> ( reinfeld myColor $ evalScoreFunction minniePieceScoreFunction s))

-- | let !(Tree _ (Tree _ (Tree _ []:_):_)) = chopTree (allMovesFrom initialState) 2
chopTree :: Tree a -> Int -> Tree a
chopTree (Tree x _) 0 = Tree x []
chopTree (Tree x xs) n = Tree x $ map (\t -> chopTree t (n - 1)) xs

-- madMax

