module Image (write, writeFloating) where

import Codec.BMP
import Data.ByteString hiding (map)

write :: (Integral a) => String -> Int -> Int -> [a] -> IO()
write filename width height buffer = writeBMP filename bmp
  where
    bmp = packRGBA32ToBMP width height rgba
    rgba = Data.ByteString.pack $ map fromIntegral buffer

writeFloating :: (RealFrac a) => String -> Int -> Int -> [a] -> IO()
writeFloating filename width height buffer = write filename width height $ map round buffer
