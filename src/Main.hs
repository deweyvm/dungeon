{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs #-}

import Prelude hiding (concat, all, and)
import Codec.Picture
import System.Random
import Data.Foldable(and)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Control.Applicative
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.Flood

type Color = PixelRGBA8

black :: Color
black = PixelRGBA8 0 0 0 255

darkGreen :: Color
darkGreen = PixelRGBA8 0 128 0 255


makeRandom :: Int -> Int -> Array2d Bool
makeRandom cols rows =
    Array2d cols rows (Vec.fromList rand)
    where rand = take (cols*rows) $ randoms (mkStdGen 0)

bool2Pixel :: Bool -> Color
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
vertStrip b mods =
    (<$*>) (\(i, j) p -> select p b (i `mod` mods == 0 || j `mod` mods == 0))

clearBorder :: Int -> Array2d Bool -> Array2d Bool
clearBorder thick arr@(Array2d cols rows _) =
    (\(i, j) p -> i < thick || j < thick || i > cols - thick - 1 || j > rows - thick - 1 || p) <$*> arr

-- \n f x -> execState (replicateM n (modify f)) x
-- use the ((->)e) monad to compose the lists of functions

coordSet2BoolArray2d :: Int -> Int -> Set.Set Point -> Array2d Bool
coordSet2BoolArray2d cols rows indices =
    tabulate cols rows False (\pt -> Set.member pt indices)

randColors :: [Color]
randColors =
    (\(r, g, b) -> PixelRGBA8 r g b 255) <$> colors
    where colors = zip3 (randoms (mkStdGen 0)) (randoms (mkStdGen 1)) (randoms (mkStdGen 3))


saveMap :: FilePath -> Array2d Color -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path $ generateImage (getOrElse arr black) cols rows

toPixelArray :: Int -> Int -> [(Color, Array2d Bool)] -> Array2d Color
toPixelArray cols rows arrs =
    tabulate cols rows black (\pt -> case fst <$> List.find (\(c, arr) -> and $ geti arr pt
                      ) arrs of
                                        Just c -> c
                                        Nothing -> black)


main :: IO ()
main = do
    let cols = 200
    let rows = 200
    let initial :: Array2d Bool = makeRandom cols rows
    let permuted = foldr (.) id [ occuCount 7
                                , clearBorder 10
                                , vertStrip True 4
                                , occuCount 5
                                ] initial
    let flooded = zip randColors $ coordSet2BoolArray2d cols rows <$> floodAll id permuted
    return ()

    saveMap "test.png" $ toPixelArray cols rows flooded
