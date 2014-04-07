{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Picture
import qualified Data.Vector as Vec
import Data.Maybe
import Data.Function
import System.Random
import Data.Word
data Array2d elt = Array2d Int Int (Vec.Vector elt)

select :: a -> a -> Bool -> a
select f t p = if p then t else f

unsafeGet :: Array2d a -> Int -> a
unsafeGet (Array2d _ _ v) k = v Vec.! k

get :: Array2d a -> Int -> Int -> Maybe a
get arr@(Array2d cols rows _) i j =
    select (Just $ unsafeGet arr $ fromCoords arr i j)
           Nothing
           (i < 0 || i > cols - 1 || j < 0 || j > rows - 1)

toCoords :: Array2d a -> Int -> (Int, Int)
toCoords (Array2d cols _ _) k = (k `mod` cols, k `quot` cols)

fromCoords :: Array2d a -> Int -> Int -> Int
fromCoords (Array2d cols _ _) i j = i + j* cols

aimap :: (Int -> Int -> a -> b) -> Array2d a -> Array2d b
aimap f arr@(Array2d cols rows v) = Array2d cols rows $ mapped
    where zipped = Vec.zip (Vec.fromList [0..(cols*rows)-1]) v
          mapped = Vec.map (\(k, p) -> let (x, y) = toCoords arr k in f x y p) zipped

amap :: (a -> b) -> Array2d a -> Array2d b
amap f arr@(Array2d cols rows v) = Array2d cols rows $ Vec.map f v

zipWithIndex :: Array2d a -> Array2d (Int, Int, a)
zipWithIndex arr = aimap (,,) arr


tabulate :: Int -> Int -> a -> (Int -> Int -> a) -> Array2d a
tabulate cols rows initial f =
    aimap (\x y _ -> f x y) base
    where base = Array2d cols rows $ Vec.replicate (cols*rows) initial

saveMap :: FilePath -> Array2d PixelRGBA8 -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path img
    where img = generateImage (\x y -> fromMaybe (PixelRGBA8 0 0 0 255) (get arr x y)) cols rows

divf :: Int -> Int -> Float
divf = (/) `on` fromIntegral

pixToColor :: Int -> Int -> Int -> Int -> PixelRGBA8
pixToColor cols rows x y =
    let toInt :: Int -> Word8 = fromIntegral in
    let color = (toInt. truncate) $ 128 * ((x `divf` cols) + (y `divf` rows)) in
        PixelRGBA8 color color color 255

makeRandom :: Int -> Int -> Array2d Bool
makeRandom cols rows =
    let rand = take (cols*rows) $ randoms (mkStdGen 0) in
                   Array2d cols rows (Vec.fromList rand)

bool2Pixel :: Bool -> PixelRGBA8
bool2Pixel = select (PixelRGBA8 0 0 0 255) (PixelRGBA8 0 128 0 255)

main :: IO ()
main = do
    let cols = 500
    let rows = 500
    let img = tabulate cols rows (PixelRGBA8 0 0 255 255) $ pixToColor cols rows
    let img2 = amap bool2Pixel $ makeRandom cols rows

    saveMap "test.png" img2
    putStrLn "this is a test"
