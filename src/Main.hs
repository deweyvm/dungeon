{-# LANGUAGE ScopedTypeVariables #-}

import Labyrinth.Generator
import Labyrinth.Util
import Labyrinth.Pathing.Util
import qualified Labyrinth.Pathing.AStar as A
import qualified Labyrinth.Pathing.Dijkstra as D
import qualified Labyrinth.Pathing.JumpPoint as J
import System.Random


data Search = Dijkstra | JumpPoint | AStar


search :: Search
search = Dijkstra

main :: IO ()
main = do
    let useGlobalRng = False
    seed <- select (return 1) randomIO useGlobalRng
    putStrLn $ "Seed: " ++ show seed
    let heuristic = ((/ 1.5) .: euclid)
    let pfind  = case search of
                    Dijkstra -> D.pfind
                    AStar -> A.pfind heuristic
                    JumpPoint -> J.pfind heuristic

    doSimple pfind seed
