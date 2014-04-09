{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (concat, foldl)
import Codec.Picture
import System.Random
import Data.Word
import Data.Maybe
import qualified Data.Vector as Vec
import Dungeon.Util
import Dungeon.Data.Array2d

saveMap :: FilePath -> Array2d PixelRGBA8 -> IO ()
saveMap path arr@(Array2d cols rows _) =
    writePng path img
    where img = generateImage (getOrElse arr (PixelRGBA8 0 0 0 255) ) cols rows



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

getOccupants :: Array2d a -> Int -> Int -> [a]
getOccupants arr i j =
    catMaybes $ map (uncurry (get arr)) [ (i, j)
                                        , (i, (j - 1))
                                        , (i, (j + 1))
                                        , ((i - 1), j)
                                        , ((i + 1), j)
                                        , ((i - 1), (j - 1))
                                        , ((i + 1), (j + 1))
                                        , ((i - 1), (j + 1))
                                        , ((i + 1), (j - 1))
                                        ]


countOccupants :: (a -> Bool) -> Array2d a -> Int -> Int -> Int
countOccupants f = (count f) .:: (getOccupants)

runMachine :: Array2d Bool -> Array2d Bool
runMachine arr = imap (\i j _ -> (countOccupants id arr i j) >= 5) arr

main :: IO ()
main = do
    let cols = 200
    let rows = 200
    let initial = makeRandom cols rows
    let permuted = iterate runMachine initial !! 4
    let img2 = fmap bool2Pixel permuted

    saveMap "test.png" img2
