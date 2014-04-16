{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs, BangPatterns #-}

import Prelude hiding (concat, all, and)
import Codec.Picture
import System.Random
import Data.Maybe
import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Control.Applicative
import Control.Parallel.Strategies
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.Flood
import Labyrinth.Path

type Color = (Word8, Word8, Word8, Word8)

black :: Color
black = (0, 0, 0, 255)

white :: Color
white = (255, 255, 255, 255)

makeRandom :: Int -> Int -> Int -> Array2d Bool
makeRandom seed cols rows =
    Array2d cols rows (Vec.fromList rand)
    where rand = take (cols*rows) $ randoms (mkStdGen seed)

getOccupants :: Array2d a -> Point -> [a]
getOccupants arr (i, j) =
    cat [ (i + x, j + y) | x <- [-1..1], y <- [-1..1] ]
    where cat = catMaybes . map (geti arr)


countOccupants :: (a -> Bool) -> Array2d a -> Point -> Int
countOccupants f = (count f) .: (getOccupants)

occuCount :: Int -> Array2d Bool -> Array2d Bool
occuCount k arr = (\pt _ -> (countOccupants id arr pt) >= k) <$*> arr

not' :: Array2d Bool -> Array2d Bool
not' = (<$>) not

vertStrip :: Bool -> Int -> Array2d Bool -> Array2d Bool
vertStrip b mods =
    (<$*>) (\(i, j) p -> select p b (i `mod` mods == 0 || j `mod` mods == 0))

clearBorder :: Int -> Array2d Bool -> Array2d Bool
clearBorder thick arr@(Array2d cols rows _) =
    (\(i, j) p -> i < thick || j < thick || i > cols - thick - 1 || j > rows - thick - 1 || p) <$*> arr

-- \n f x -> execState (replicateM n (modify f)) x

randColors :: Int -> [Color]
randColors seed =
 (\(r, g, b) -> (r, g, b, 255)) <$> colors
   where colors = zip3 (rand id) (rand (+1)) (rand (+2))
         rand f = (randoms . mkStdGen . f) seed

saveMap :: FilePath -> Array2d Color -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path $ generateImage (getOrElse colors (PixelRGBA8 0 0 0 255)) cols rows
    where colors = (\(r, g, b, a) -> PixelRGBA8 r g b a) <$> arr

toPixelArray :: Int -> Int -> [(Color, Set.Set Point)] -> Array2d Color
toPixelArray cols rows pts =
    tabulate cols rows black (\pt -> case found pt of
                                        Just c -> c
                                        Nothing -> black)
    where found :: Point -> Maybe Color
          found pt = fst <$> List.find (\(_, set) -> Set.member pt set) pts

minMaxView :: Set.Set a -> Maybe (a, a, Set.Set a)
minMaxView set = do
    (x, rest) <- Set.maxView set
    (y, rest2) <- Set.minView rest
    return (x, y, rest2)

addPath :: Array2d Bool -> (Color, Set.Set Point) -> [(Color, Set.Set Point)]
addPath arr tup@(color, area) =
    case minMaxView area of
        Just (x, y, rest) ->
            let path = pfind arr x y in
                case path of
                    Right pts -> let set = Set.fromList pts in [(color, area Set.\\ set), (white, set)]
                    Left _ -> undefined
        Nothing -> [tup]

(<$=>) :: (NFData b) => (a -> b) -> [a] -> [b]
f <$=> ls = (parMap rdeepseq) f ls

main :: IO ()
main = do
    seed :: Int <- randomIO
    let cols = 200
    let rows = 200
    let initial :: Array2d Bool = makeRandom seed cols rows
    let permuted = foldr (.) id [ not'
                                , occuCount 7
                                , clearBorder 10
                                , vertStrip True 4
                                , occuCount 5
                                ] initial
    let flooded = zip (randColors seed) $ floodAll id permuted
    let pathed = List.concat $ (addPath permuted) <$=> flooded
    let arr = toPixelArray cols rows pathed

    let patharr = (tabulate 10 10 False (\(x, y) -> (x == 0 || y == 0 || x == 9 || y == 9)))
    let start = (0, 0) :: Point
    let end = (9, 9) :: Point
    let path = either (\_ -> []) id $ pfind patharr start end
    print path
    saveMap "test.png" $ arr
