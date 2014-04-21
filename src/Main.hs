{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs, BangPatterns, FlexibleInstances, TypeSynonymInstances,MultiParamTypeClasses #-}


import Prelude hiding (concat, all, and, foldr)
import Codec.Picture
import System.Random
import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Maybe
import Control.Applicative
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.PathGraph
import Labyrinth.Generator
import qualified Labyrinth.Pathing.JumpPoint as J
import qualified Labyrinth.Pathing.AStar as A
import qualified Labyrinth.Machine2d as M
import qualified Labyrinth.Flood as F
import Labyrinth.Instances()
import Debug.Trace



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

data PathRegion a = PathRegion a           -- start node
                               a           -- end node
                               (Set.Set a) -- nodes not covered by path
                               [a]         -- path

prContains :: Point -> PathRegion Point -> Bool
prContains pt (PathRegion start end open path) =
    pt == start || pt == end || Set.member pt open || elem pt path

toPixelArray :: Int -> Int -> Int -> [PathRegion Point] -> Array2d Color
toPixelArray seed cols rows regions =
    tabulate cols rows black (\pt -> maybe black id (found pt <$> findReg pt))--getOrElse cc black in undefined)
    where colors = zip (randColors seed) regions
          findReg pt = List.find (\(color, reg) -> prContains pt reg) colors
          found :: Point -> (Color, PathRegion Point) -> Color
          found pt (color, (PathRegion start end rest path))
              | start == pt = (0, 255, 0)
              | end == pt = (255, 0, 0)
              | Set.member pt rest = color
              | elem pt path = white

createPath :: Array2d Bool -> Set.Set Point -> Maybe (Point, Point, Set.Set Point, [Point])
createPath arr area =
    case minMaxView area of
        Just (x, y, rest) ->
            case J.pfind arr x y of
                Right pts ->
                     Just (x, y, Set.difference rest (Set.union (Set.fromList [x, y]) (Set.fromList pts)), pts)
                -- this case should be impossible if pfind is correct
                Left s -> trace ("Failed to find path: " ++ s) Nothing
        Nothing -> Nothing
getOpen :: Open a => Array2d a -> Set.Set Point
getOpen arr = Set.fromList $ foldli (\xs (pt, x) -> (if isOpen x then (pt:) else id) xs) [] arr


largest :: [Set.Set Point] -> Set.Set Point
largest s = List.maximumBy setSize s
    where setSize s0 s1 =  Set.size s0 `compare` Set.size s1

mkPathRegion :: (a, a, Set.Set a, [a]) -> PathRegion a
mkPathRegion (start, end, open, path) = PathRegion start end open path

main :: IO ()
main = do
    --seed :: Int <- randomIO
    --let seed = -135580466 -- 50, 50
    let seed = 2028449052
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
    saveMap "flood.png" $ arr
