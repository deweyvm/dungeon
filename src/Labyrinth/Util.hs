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
import Control.Arrow(first, second)
import qualified Data.Set as Set
import Control.Parallel.Strategies

-- | Alias for a pair of integers.
type Point = (Int,Int)

-- | Trace then return the same value.
myTrace :: Show a => a -> a
myTrace x = traceShow x x

-- | Returns a Float approximation of the divison of two Int's.
divf :: Int -> Int -> Float
divf = (/) `on` fromIntegral

-- | Compose a 2-argument function with a 1-argument function.
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

-- | Compose a 3-argument function with a 1-argument function.
(.::) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.::) = (.:) . (.)

-- | Count the number of elements in a list satisfying a predicate.
count ::  (a -> Bool) -> [a] -> Int
count f = length . (filter f)

-- | If expression function.
select :: a    -- ^ false expr
       -> a    -- ^ true expr
       -> Bool -- ^ predicate
       -> a    -- ^ the result of @if p then t else f@
select f t p = if p then t else f

-- | Parallel fmap.
(<$=>) :: (NFData b) => (a -> b) -> [a] -> [b]
f <$=> ls = (parMap rdeepseq) f ls

-- | Retrieves the minimal and maximal elements of the set along with the
-- set stripped of those elements or Nothing if not enough members exist
-- in the given set.
minMaxView :: Set.Set a -> Maybe (a, a, Set.Set a)
minMaxView set = do
    (x, rest) <- Set.maxView set
    (y, rest2) <- Set.minView rest
    return (x, y, rest2)

catEithers :: [Either a b] -> ([a], [b])
catEithers [] = (,) [] []
catEithers (Right x : xs) = second (x:) $ catEithers xs
catEithers (Left x  : xs) = first (x:) $ catEithers xs
