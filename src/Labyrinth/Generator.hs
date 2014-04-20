{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Labyrinth.Generator
Description : pathfinding testcase generator
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Generates maps for checking the validity of pathfinding algorithms.
-}
module Labyrinth.Generator(printCase) where
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import Labyrinth.Data.Array2d
import Labyrinth.Util

printArray :: (a -> String) -> Array2d a -> String
printArray f arr =
    let (Array2d _ _ vec) = (\(x, y) p -> select "" "\n" (x == 0 && y /= 0)  ++ (f p)) <$*> arr in
    concat (Vec.toList vec)

printSet :: Bool -> (Bool -> String) -> Int -> Int -> Set.Set Point -> String
printSet x f cols rows set =
    let arr = tabulate cols rows x (\pt -> Set.member pt set) in
    printArray f arr

printPoint :: Point -> String
printPoint (i, j) = (show i ++ "," ++ show j)

printCase :: Set.Set Point -> Int -> Int -> (Bool -> String) -> String
printCase set rows cols f = do
    case minMaxView set of
        Just (x, y, _) ->
            printPoint x ++ "\n" ++ printPoint y ++ "\n" ++  printSet False f rows cols set
        Nothing -> ""
