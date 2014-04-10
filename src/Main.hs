{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

import Prelude hiding (concat, foldl, all)
import Codec.Picture
import System.Random
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Data.Sequence hiding (take, zip, filter)
import Control.Applicative
import Control.Monad
import Control.Arrow((***))
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
    where cat = catMaybes . map (geti arr)


countOccupants :: (a -> Bool) -> Array2d a -> Int -> Int -> Int
countOccupants f = (count f) .:: (getOccupants)

occuCount :: Int -> Array2d Bool -> Array2d Bool
occuCount k arr = (\i j _ -> (countOccupants id arr i j) >= k) <$*> arr

vertStrip :: Bool -> Int -> Array2d Bool -> Array2d Bool
vertStrip b mods = (<$*>) (\i j p -> select p b (i `mod` mods == 0 || j `mod` mods == 0))

neighbors :: [Point]
neighbors = [ (x, y) | x <- [-1..1], y <- [-1..1], not (x == 0 && y == 0) ]

getNeighbors :: Point -> [Point]
getNeighbors (i, j) = map ((i +) *** (j +)) neighbors
-- (\(x, y) -> (i + x, j + y)) === (i +) *** (j +)

--

data Flood = Flood (Set.Set Point) (Seq Point)

newFlood :: Point -> Flood
newFlood = liftM2 Flood Set.singleton singleton

filterPoints :: forall a . Array2d a -> ((Point,a) -> Bool) -> [Point] -> [Point]
filterPoints arr f pts =
    let x = zip pts $ (geti arr) <$> pts in
    let y = catMaybes $ pairMaybe <$> x in
    fst <$> (filter f y)
  where pairMaybe :: forall c b . (c, Maybe b) -> Maybe (c, b)
        pairMaybe (x, Just y) = Just (x, y)
        pairMaybe (_, Nothing) = Nothing

floodHelper :: (a -> Bool) -> Array2d a -> Flood -> Set.Set Point
floodHelper _ _ (Flood pts (viewl -> EmptyL)) = pts
floodHelper f arr (Flood pts (viewl -> pt :< work)) =
    let ns = filterPoints arr (\(p, elt) -> f elt && not (Set.member p pts)) (getNeighbors pt) in
    let newQueue = (fromList ns) >< work in
    let newPoints = (pts `Set.union` Set.fromList ns) in
    floodHelper f arr (Flood newPoints newQueue)

floodFill :: Point -> (a -> Bool) -> Array2d a -> Set.Set Point
floodFill pt f arr =
    floodHelper f arr $ newFlood pt

-- \n f x -> execState (replicateM n (modify f)) x
-- use the ((->)e) monad to compose the lists of functions

coordList2BoolArray2d :: Int -> Int -> Set.Set Point -> Array2d Bool
coordList2BoolArray2d cols rows indices =
    tabulate cols rows False (\i j -> Set.member (i, j) indices)



main :: IO ()
main = do
    let cols = 200
    let rows = 200
    let initial :: Array2d Bool = makeRandom cols rows
    let permuted = foldr (.) id [occuCount 5, vertStrip True 15, vertStrip False 16, occuCount 5] initial
    let flooded = case find id permuted of
                    Nothing -> permuted
                    Just (x, y, _) -> coordList2BoolArray2d cols rows $ floodFill (x, y) id permuted

    let img = bool2Pixel <$> flooded

    saveMap "test.png" img
