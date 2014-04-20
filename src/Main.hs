{-# LANGUAGE ScopedTypeVariables, ViewPatterns, InstanceSigs, BangPatterns, FlexibleInstances, TypeSynonymInstances,MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude hiding (concat, all, and, foldr, any)
import Codec.Picture
import System.Random
import Data.Word
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Foldable(any)
import Control.Applicative
import Control.Monad
import Control.Arrow((***))
import Labyrinth.Util
import Labyrinth.Data.Array2d
import Labyrinth.PathGraph
import Labyrinth.Pathing.Util
import Labyrinth.Generator
import qualified Labyrinth.Pathing.JumpPoint as J
import qualified Labyrinth.Pathing.AStar as A
import qualified Labyrinth.Machine2d as M
import qualified Labyrinth.Flood as F
import Debug.Trace

instance Open Bool where
    isOpen = id

neighbors8 :: [Point]
neighbors8 = ns
    where ns = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

-- | Retrieves the 8 neighbors of a 2d point
getNeighbors8 :: Point -> [Point]
getNeighbors8 (i, j) = ((i +) *** (j +)) <$> neighbors8
-- (\(x, y)-> (i + x, j + y)) === (i +) *** (j +)


instance Open a => PathGraph (Array2d a) Point where
   getNeighbors arr pt = ap (,) (euclid pt) <$> ns
       where ns = filter open $ getNeighbors8 pt
             open = (any isOpen) . (geti arr)

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

addPath :: Array2d Bool -> (Color, Set.Set Point) -> [(Color, Set.Set Point)]
addPath arr tup@(color, area) =
    case minMaxView area of
        Just (x, y, rest) ->
            case J.pfind arr x y of
                Right pts -> [(color, rest Set.\\ set), (white, set)]
                   where set = Set.fromList pts
                -- this case should be impossible if pfind is correct
                Left s -> trace ("Failed to find path: " ++ s) [tup]
        Nothing -> [tup]

getOpen :: Open a => Array2d a -> Set.Set Point
getOpen arr = Set.fromList $ foldli (\xs (pt, x) -> (if isOpen x then (pt:) else id) xs) [] arr


largest :: [(Color, Set.Set Point)] -> [(Color, Set.Set Point)]
largest s = (:[]) $ List.maximumBy setSize s
    where setSize (_, s0) (_, s1) =  Set.size s0 `compare` Set.size s1



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
    let flooded = largest $ zip (randColors seed) $ F.simpleFloodAll permuted open

    putStrLn $ printCase (snd (flooded !! 0)) cols rows (select "x" "0")

    let pathed = List.concat $ (addPath permuted) <$=> flooded

    let arr = toPixelArray cols rows pathed

    saveMap "mask.png" $ (select white black) <$> permuted
    saveMap "flood.png" $ arr
