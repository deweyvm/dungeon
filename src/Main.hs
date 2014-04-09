{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (concat, foldl)
import Codec.Picture
import System.Random
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Labyrinth.Util
import Labyrinth.Data.Array2d

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

darkGreen :: PixelRGBA8
darkGreen = PixelRGBA8 0 128 0 255

saveMap :: FilePath -> Array2d PixelRGBA8 -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path $ generateImage (getOrElse arr black) cols rows

makeRandom :: Int -> Int -> Array2d Bool
makeRandom cols rows =
    let rand = take (cols*rows) $ randoms (mkStdGen 0) in
                   Array2d cols rows (Vec.fromList rand)

bool2Pixel :: Bool -> PixelRGBA8
bool2Pixel = select black darkGreen


getOccupants :: Array2d a -> Int -> Int -> [a]
getOccupants arr i j =
    cat [ (i + x, j + y) | x <- [-1..1], y <- [-1..1] ]
    where cat = catMaybes . map (uncurry (get arr))


countOccupants :: (a -> Bool) -> Array2d a -> Int -> Int -> Int
countOccupants f = (count f) .:: (getOccupants)

occuCount :: Int -> Array2d Bool -> Array2d Bool
occuCount k arr = imap (\i j _ -> (countOccupants id arr i j) >= k) arr

neighbors :: [(Int,Int)]
neighbors = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

getNeighbors :: (Int, Int) -> [(Int,Int)]
getNeighbors (i, j) = [ (i + x, i + y) | (x, y) <- neighbors ]
--
--floodFill :: (Int, Int) -> (a -> Bool) -> Array2d a -> [(Int,Int)]
--floodFill initial f arr =
--    let

--floodHelper :: Set (Int,Int)
-- \n f x -> execState (replicateM n (modify f)) x
-- use the ((->)e) monad to compose the lists of functions

main :: IO ()
main = do
    let cols = 200
    let rows = 200
    let initial :: Array2d Bool = makeRandom cols rows
    let permuted = foldr (.) id [occuCount 5, occuCount 5] initial
    let img = fmap bool2Pixel permuted

    saveMap "test.png" img
