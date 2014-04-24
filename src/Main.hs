{-# LANGUAGE ScopedTypeVariables #-}

import Labyrinth.Generator
import Labyrinth.Util
import qualified Labyrinth.Pathing.AStar as A
import qualified Labyrinth.Pathing.Dijkstra as D
import qualified Labyrinth.Pathing.JumpPoint as J
import System.Random

main :: IO ()
main = do
    let useGlobalRng = False
    seed <- select (return 1) randomIO useGlobalRng
    putStrLn $ "Seed: " ++ show seed
    doSimple A.pfind seed
