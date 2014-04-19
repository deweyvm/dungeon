{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs, BangPatterns, FlexibleInstances, TypeSynonymInstances,MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (concat, all, and, foldr)
import Codec.Picture
import System.Random
import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Maybe
import Control.Applicative
import Control.Arrow((***))
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.Pathing.AStar
import qualified Labyrinth.Machine2d as M
import qualified Labyrinth.Flood as F
import Debug.Trace

instance Open Bool where
    isOpen = id


neighbors8 :: [Point]
neighbors8 = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

-- | Retrieves the 8 neighbors of a 2d point
getNeighbors8 :: Point -> [Point]
getNeighbors8 (i, j) = map ((i +) *** (j +)) neighbors8
-- (\(x, y)-> (i + x, j + y)) === (i +) *** (j +)

euclid :: Point -> Point -> Float
euclid (i, j) (x, y) =  (sqrt (xx + yy))
        where xx = sq (x - i)
              yy = sq (y - j)
              sq= (** 2) . fromIntegral

instance Open a => PathGraph (Array2d a) Point where
    getNeighbors arr pt = (\p -> (p, euclid p pt)) <$> fst <$> filtered
        where filtered = filter (isOpen . snd) ns
              ns = catMaybes $ (geti (zipWithIndex arr)) <$> getNeighbors8 pt

instance Metric Point where
    guessLength = (/ 1.5) .: euclid


type Color = (Word8, Word8, Word8)

black :: Color
black = (0, 0, 0)

white :: Color
white = (255, 255, 255)

makeRandom :: Int -> Int -> Int -> Array2d Bool
makeRandom seed cols rows =
    Array2d cols rows (Vec.fromList rand)
    where rand = take (cols*rows) $ randoms (mkStdGen seed)

randColors :: Int -> [Color]
randColors seed =
   zip3 (rand id) (rand (+1)) (rand (+2))
   where rand f = (randoms . mkStdGen . f) seed

saveMap :: FilePath -> Array2d Color -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path $ generateImage (getOrElse colors (PixelRGBA8 0 0 0 255)) cols rows
    where colors = (\(r, g, b) -> PixelRGBA8 r g b 255) <$> arr

toPixelArray :: Int -> Int -> [(Color, Set.Set Point)] -> Array2d Color
toPixelArray cols rows pts =
    tabulate cols rows black (maybe black id . found)
    where found :: Point -> Maybe Color
          found pt = fst <$> List.find (\(_, set) -> Set.member pt set) pts

-- | Retrieves the minimal and maximal elements of the set along with the
-- set stripped of those elements or Nothing if not enough members exist
-- in the given set
minMaxView :: Set.Set a -> Maybe (a, a, Set.Set a)
minMaxView set = do
    (x, rest) <- Set.maxView set
    (y, rest2) <- Set.minView rest
    return (x, y, rest2)

addPath :: Array2d Bool -> (Color, Set.Set Point) -> [(Color, Set.Set Point)]
addPath arr tup@(color, area) =
    case minMaxView area of
        Just (x, y, rest) ->
            case pfind arr x y of
                Right pts -> [(color, rest Set.\\ set), (white, set)]
                   where set = Set.fromList pts
                Left _ -> trace "Failed to find path" [tup]
        Nothing -> [tup]

getOpen :: Open a => Array2d a -> Set.Set Point
getOpen arr = Set.fromList $ foldli (\xs (pt, x) -> if isOpen x then (pt:xs) else xs) [] arr


main :: IO ()
main = do
    --seed :: Int <- randomIO
    let seed = 0
    let cols = 100
    let rows = 100
    let initial = makeRandom seed cols rows
    let permuted = initial M.<.> [ M.negate
                                 , M.occuCount 7
                                 , M.clearBorder 10
                                 , M.vertStrip True 4
                                 , M.occuCount 5
                                 ]
    let open = getOpen permuted
    let flooded = zip (randColors seed) $ F.simpleFloodAll permuted open
    let pathed = List.concat $ (addPath permuted) <$=> flooded
    let arr = toPixelArray cols rows pathed

    saveMap "mask.png" $ (select white black) <$> permuted
    saveMap "flood.png" $ arr
