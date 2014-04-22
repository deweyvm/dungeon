{-# LANGUAGE ScopedTypeVariables #-}

import Labyrinth.Generator
import Labyrinth.Util
import qualified Labyrinth.Pathing.JumpPoint as J
import System.Random

main :: IO ()
main = do
    let useGlobalRng = False
    seed <- select randomIO (return 0) useGlobalRng
    putStrLn $ "Seed: " ++ show seed
    doSimple J.pfind seed

