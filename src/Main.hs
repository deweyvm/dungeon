{-# LANGUAGE ScopedTypeVariables #-}

import Labyrinth.Generator
import Labyrinth.Util
import Labyrinth.Pathing.Util
import qualified Labyrinth.Pathing.JumpPoint as J
import System.Random

main :: IO ()
main = do
    let useGlobalRng = False
    seed <- select (return 1) randomIO useGlobalRng
    putStrLn $ "Seed: " ++ show seed
    doSimple J.pfind seed
    print $ expand (10,10) (0,20)
    print $ expand (10,10) (10,10)
    print $ expand (10,10) (10,20)
    print $ expand (30,10) (10,10)
