{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

import Prelude hiding (concat, foldl, all)
import Codec.Picture
import System.Random
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Control.Applicative
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.Flood

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

darkGreen :: PixelRGBA8
darkGreen = PixelRGBA8 0 128 0 255

saveMap :: FilePath -> Array2d PixelRGBA8 -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path $ generateImage (getOrElse arr black) cols rows

makeRandom :: Int -> Int -> Array2d Bool
makeRandom cols rows =
    Array2d cols rows (Vec.fromList rand)
    where rand = take (cols*rows) $ randoms (mkStdGen 0)

bool2Pixel :: Bool -> PixelRGBA8
bool2Pixel = select black darkGreen


getOccupants :: Array2d a -> Point -> [a]
getOccupants arr (i, j) =
    cat [ (i + x, j + y) | x <- [-1..1], y <- [-1..1] ]
    where cat = catMaybes . map (geti arr)


countOccupants :: (a -> Bool) -> Array2d a -> Point -> Int
countOccupants f = (count f) .: (getOccupants)

occuCount :: Int -> Array2d Bool -> Array2d Bool
occuCount k arr = (\pt _ -> (countOccupants id arr pt) >= k) <$*> arr

vertStrip :: Bool -> Int -> Array2d Bool -> Array2d Bool
vertStrip b mods = (<$*>) (\(i, j) p -> select p b (i `mod` mods == 0 || j `mod` mods == 0))


-- \n f x -> execState (replicateM n (modify f)) x
-- use the ((->)e) monad to compose the lists of functions

coordList2BoolArray2d :: Int -> Int -> Set.Set Point -> Array2d Bool
coordList2BoolArray2d cols rows indices =
    tabulate cols rows False (\pt -> Set.member pt indices)



main :: IO ()
main = do
    let cols = 200
    let rows = 200
    let initial :: Array2d Bool = makeRandom cols rows
    let permuted = foldr (.) id [occuCount 5, vertStrip True 4, occuCount 5] initial
    let flooded = case find id permuted of
                    Nothing -> permuted
                    Just (pt, _) -> coordList2BoolArray2d cols rows $ floodFill pt id permuted

    let img = bool2Pixel <$> flooded

    saveMap "test.png" img
