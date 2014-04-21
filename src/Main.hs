{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs, BangPatterns, FlexibleInstances, TypeSynonymInstances,MultiParamTypeClasses #-}


import Prelude hiding (concat, all, and, foldr)

import Labyrinth.Generator

main :: IO ()
main = do
    putStrLn "hello work"
    doSimple
    --seed :: Int <- randomIO
    --let seed = -135580466 -- 50, 50
    {-let seed = 2028449052
    let cols = 100
    let rows = 100
    let initial = makeRandom seed cols rows
    let permuted = initial M.<.> [ M.occuCount 5
                                 , M.vertStrip True 4
                                 , M.occuCount 5
                                 ]
    let open = getOpen permuted
    let flooded = ((:[]) . largest) $ F.simpleFloodAll permuted open

    putStrLn $ printCase ((flooded !! 0)) cols rows (select "x" "0")

    let paths = catMaybes $ (createPath permuted) <$=> flooded
    let pathRegions = mkPathRegion <$> paths
    let arr = toPixelArray seed cols rows pathRegions

    saveMap "mask.png" $ (select white black) <$> permuted
    saveMap "flood.png" $ arr-}
