{-# LANGUAGE BangPatterns #-}

module Event where

import Config
import Quickhull
import World

import Control.Monad
import Graphics.Gloss.Interface.IO.Interact
import System.Exit

import Data.Array.Accelerate                                        ( toList )
import Data.Array.Accelerate.LLVM.Native
-- import Data.Array.Accelerate.LLVM.PTX
-- import Data.Array.Accelerate.Interpreter


handleEvent :: Event -> World -> IO World
handleEvent event world
  | EventKey key Down _ _ <- event
  = case key of
      SpecialKey KeyEsc   -> exitSuccess
      _                   -> return world { previousEvent = Just key }

  | EventKey key Up _ _ <- event
  , Just oldKey <- previousEvent world
  , key == oldKey
  = case key of
      SpecialKey k
        | isAdvanceKey k  -> do
          s' <- advance (state world)
          return world { previousEvent = Nothing, state = s' }
        | isPreviousKey k -> return world { previousEvent = Nothing, state = previous (state world) }
      _                   -> return world { previousEvent = Nothing }

  | EventResize (x,y) <- event
  = return $ world { screenSize = (min x y) - _PAD }

  | otherwise
  = return world

isAdvanceKey :: SpecialKey -> Bool
isAdvanceKey KeySpace = True
isAdvanceKey KeyEnter = True
isAdvanceKey KeyRight = True
isAdvanceKey KeyDown  = True
isAdvanceKey _        = False

isPreviousKey :: SpecialKey -> Bool
isPreviousKey KeyDelete    = True
isPreviousKey KeyBackspace = True
isPreviousKey KeyLeft      = True
isPreviousKey KeyUp        = True
isPreviousKey _            = False

advance :: S -> IO S
advance s =
  case s of
    S0 p     -> do
      putStrLn "== initial partition"
      return $ S1 (initialPartition' p) False s

    S1 p c _ ->
      if c then return s
           else do
             putStrLn "== partition"
             let p' = partition' p
                 c' = and (toList (fst p'))
             when c' $ putStrLn "== COMPLETE"
             return $ S1 p' c' s
  where
    !initialPartition' = runN initialPartition
    !partition'        = runN partition

previous :: S -> S
previous s =
  case s of
    S0 _      -> s
    S1 _ _ s0 -> s0

