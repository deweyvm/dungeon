{-# LANGUAGE ScopedTypeVariables #-}

import Labyrinth.Generator
import qualified Labyrinth.Pathing.JumpPoint as J
import System.Random

main :: IO ()
main = do
    seed :: Int <- randomIO
    let seed = 0
    putStrLn $ "Seed: " ++ show seed
    doSimple J.pfind seed

