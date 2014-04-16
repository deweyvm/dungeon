{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Labyrinth.Util
Description : utilities
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

General utility functions for labyrinth routines.
-}
module Labyrinth.Util where

import Debug.Trace
import Data.Function
import Control.Parallel.Strategies

-- | Alias for a pair of integers
type Point = (Int,Int)

-- | Trace then return the same value
myTrace :: Show a => a -> a
myTrace x = traceShow x x

-- | Returns a Float approximation of the divison of two Int's
divf :: Int -> Int -> Float
divf = (/) `on` fromIntegral

-- | Compose a 2-argument function with a 1-argument function
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

-- | Compose a 3-argument function with a 1-argument function
(.::) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.::) = (.:) . (.)

-- | Count the numer of elements in a list
count ::  (a -> Bool) -> [a] -> Int
count f = length . (filter f)

-- | If expression function
select :: a    -- ^ false expr
       -> a    -- ^ true expr
       -> Bool -- ^ predicate
       -> a    -- ^ the result of @if p then t else f@
select f t p = if p then t else f

-- | Parallel fmap
(<$=>) :: (NFData b) => (a -> b) -> [a] -> [b]
f <$=> ls = (parMap rdeepseq) f ls
