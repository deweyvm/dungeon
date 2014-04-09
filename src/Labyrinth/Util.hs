{-# LANGUAGE ScopedTypeVariables #-}
module Labyrinth.Util where

import Debug.Trace
import Data.Function

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
