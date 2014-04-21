{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-|
Module      : Labyrinth.Generator
Description : pathfinding testcase generator
Copyright   : (c) deweyvm 2014
License     : MIT
Maintainer  : deweyvm
Stability   : experimental
Portability : unknown

Generates maps for checking the validity of pathfinding algorithms.
-}
module Labyrinth.Generator(printCase, doSimple) where
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
import Labyrinth.Graph
import qualified Labyrinth.Machine2d as M
import qualified Labyrinth.Flood as F
import Labyrinth.Instances()
import Debug.Trace

printArray :: (a -> String) -> Array2d a -> String
printArray f arr =
    let (Array2d _ _ vec) = (\(x, y) p -> select "" "\n" (x == 0 && y /= 0)  ++ (f p)) <$*> arr in
    concat (Vec.toList vec)

printSet :: Bool -> (Bool -> String) -> Int -> Int -> Set.Set Point -> String
printSet x f cols rows set =
    let arr = tabulate cols rows x (\pt -> Set.member pt set) in
    printArray f arr

printPoint :: Point -> String
printPoint (i, j) = (show i ++ "," ++ show j)

printCase :: Set.Set Point -> Int -> Int -> (Bool -> String) -> String
printCase set rows cols f = do
    case minMaxView set of
        Just (x, y, _) ->
            printPoint x ++ "\n" ++ printPoint y ++ "\n" ++  printSet False f rows cols set
        Nothing -> ""


makeRandom :: Int -> Int -> Int -> Array2d Bool
makeRandom seed cols rows =
    Array2d cols rows (Vec.fromList rand)
    where rand = take (cols*rows) $ randoms (mkStdGen seed)


type Color = (Word8, Word8, Word8)

black :: Color
black = (0, 0, 0)

white :: Color
white = (255, 255, 255)


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

mkPathRegion :: (a, a, Set.Set a, [a]) -> PathRegion a
mkPathRegion (start, end, open, path) = PathRegion start end open path

prContains :: Point -> PathRegion Point -> Bool
prContains pt (PathRegion start end open path) =
    pt == start || pt == end || Set.member pt open || elem pt path

toPixelArray :: Int -> Int -> Int -> [PathRegion Point] -> Array2d Color
toPixelArray seed cols rows regions =
    tabulate cols rows black (\pt -> maybe black id (found pt <$> findReg pt))
    where colors = zip (randColors seed) regions
          findReg pt = List.find (\(_, reg) -> prContains pt reg) colors
          found :: Point -> (Color, PathRegion Point) -> Color
          found pt (color, (PathRegion start end rest path))
              | start == pt = (0, 255, 0)
              | end == pt = (255, 0, 0)
              | Set.member pt rest = color
              | elem pt path = white

createPath :: (Ord Point, Metric Point, Graph (Array2d Bool) Point)
           => (Array2d Bool -> Point -> Point -> Either String [Point])
           -> Array2d Bool
           -> Set.Set Point
           -> Maybe (Point, Point, Set.Set Point, [Point])
createPath pfind arr area =
    case F.getMaxDistance arr area of
        Just (x, y, d) ->
            let rest = trace ("DEPTH: " ++ show d) $ Set.difference area (Set.fromList [x, y]) in
            case pfind arr x y of
                Right pts ->
                     Just (x, y, removed, pts)
                     where removed = Set.difference rest (Set.union (Set.fromList [x, y])
                                                                    (Set.fromList pts))
                -- this case should be impossible if pfind is correct
                Left s -> trace ("Failed to find path: " ++ s) Nothing
        Nothing -> Nothing
getOpen :: Open a => Array2d a -> Set.Set Point
getOpen arr = Set.fromList $ foldli (\xs (pt, x) -> (select id (pt:) (isOpen x)) xs) [] arr


largest :: [Set.Set Point] -> Set.Set Point
largest s = List.maximumBy setSize s
    where setSize s0 s1 =  Set.size s0 `compare` Set.size s1


data Params a = Params Int Int Int (a -> a)


saveMask :: Params a -> Array2d Bool -> IO ()
saveMask _ arr = saveMap "mask.png" $ (select white black) <$> arr

saveFlooded :: Params a -> [Set.Set Point] -> IO ()
saveFlooded (Params _ cols rows _) (x:_) = do
    let _ = printCase x cols rows (select "x" "0")
    return ()

saveFlooded _ _ = return ()

savePathed :: Params a -> Array2d Color -> IO ()
savePathed _ = saveMap "flood.png"


doSimple :: (Ord Point, Metric Point, Graph (Array2d Bool) Point)
         => (Array2d Bool -> Point -> Point -> Either String [Point])
         -> Int
         -> IO ()
doSimple pfind seed = processMaze pfind saveMask saveFlooded savePathed (Params seed 20 20 transform)
    where transform = (M.<.> [ M.occuCount 7
                             , M.vertStrip True 4
                             , M.occuCount 5
                             ])

processMaze :: (Ord Point, Metric Point, Graph (Array2d Bool) Point)
            => (Array2d Bool -> Point -> Point -> Either String [Point])
            -> (Params (Array2d Bool) -> Array2d Bool -> IO ())
            -> (Params (Array2d Bool) -> [Set.Set Point] -> IO ())
            -> (Params (Array2d Bool) -> Array2d Color -> IO ())
            -> Params (Array2d Bool)
            -> IO ()
processMaze pfind processMask processFlooded processPathed p@(Params seed rows cols endo) = do
    let initial = makeRandom seed cols rows
    let permuted = endo initial
    let open = getOpen permuted
    let flooded = F.simpleFloodAll permuted open
    let biggest = ((:[]) . largest) flooded
    let paths = catMaybes $ (createPath pfind permuted) <$=> biggest
    let pathRegions = mkPathRegion <$> paths
    let arr = toPixelArray seed cols rows pathRegions
    processMask p permuted
    processFlooded p biggest
    processPathed p arr
