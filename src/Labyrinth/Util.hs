{-# LANGUAGE ScopedTypeVariables #-}
module Labyrinth.Util where

import Debug.Trace
import Data.Function
import Control.Parallel.Strategies

myTrace :: Show a => a -> a
myTrace x = traceShow x x

divf :: Int -> Int -> Float
divf = (/) `on` fromIntegral

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = ((.) . (.))

(.::) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.::) = (.:) . (.)

count ::  (a -> Bool) -> [a] -> Int
count f = length . (filter f)

select :: a -> a -> Bool -> a
select f t p = if p then t else f


(<$=>) :: (NFData b) => (a -> b) -> [a] -> [b]
f <$=> ls = (parMap rdeepseq) f ls
