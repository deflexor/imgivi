{-# LANGUAGE NoImplicitPrelude #-}

-- | Render a 'DynamicImage' as a vty 'Image' using the img2ascii algorithm.
--
-- Each terminal cell covers a @cellW × 2·cellH@ block of source pixels.
-- For each block we compute:
--   * average RGB colour  → set as both foreground and background
--   * average luminance   → map to a character from a density ramp
--
-- The character's shape (dense for dark, sparse for bright) provides
-- fine-grained perceived brightness, exactly matching @img2ascii -fb@.
module ImgVi.TerminalImage
  ( renderImage
  ) where

import Relude

import Codec.Picture
  ( DynamicImage
  , Image
  , PixelRGB8(..)
  , convertRGB8
  , imageHeight
  , imageWidth
  , pixelAt
  )
import Codec.Picture.Extra (scaleBilinear)
import qualified Graphics.Vty as Vty
import Graphics.Vty.Attributes.Color (linearColor)
import qualified Graphics.Vty.Image as VI

-- | Character ramp: dense (dark) → sparse (bright). Same as img2ascii default.
ramp :: String
ramp = "@#8&|o:*_. "

-- | Map average luminance (0–255) to a character from the ramp.
-- Matches img2ascii's @pixel_to_char@ formula.
lumaToChar :: Int -> Char
lumaToChar luma =
  let rampLen = length ramp - 1
      idx     = ceiling (fromIntegral (rampLen * luma) / 255 :: Double)
  in  fromMaybe (fromMaybe ' ' (reverse ramp !!? 0)) (drop idx ramp !!? 0)

-- | Render a dynamic image to fit within the given terminal cell dimensions,
-- preserving aspect ratio.
--
-- Algorithm:
--   1. Scale the source image to fit within @paneCols × (paneRows*2)@ pixels
--      (preserving aspect ratio) using bilinear interpolation.
--   2. Walk the scaled image cell-by-cell: compute average colour + luminance
--      of each 1×2 pixel column (two pixel rows per terminal row).
--   3. Map luminance → ramp character, set both fg and bg to the averaged colour.
--
-- The caller ('drawImageViewer') wraps the result in 'C.center' so that
-- any unused pane space becomes empty cells around the image.
renderImage :: DynamicImage -> Int -> Int -> Vty.Image
renderImage dynImg paneCols paneRows
  | paneCols <= 0 || paneRows <= 0 = VI.emptyImage
  | otherwise =
    let img     = convertRGB8 dynImg
        srcW    = imageWidth img
        srcH    = imageHeight img
        maxPxW  = paneCols
        maxPxH  = paneRows * 2
        -- Scale to fit inside pane pixels (preserve aspect ratio)
        scaleF  = min (fromIntegral maxPxW / fromIntegral srcW :: Double)
                      (fromIntegral maxPxH / fromIntegral srcH :: Double)
        outW    = max 1 (round (fromIntegral srcW * scaleF :: Double))
        outH    = max 1 (round (fromIntegral srcH * scaleF :: Double))
        outRows = (outH + 1) `div` 2
        scaled  = scaleBilinear outW outH img
    in  VI.vertCat $ map (\r -> buildRow scaled r outW outH) [0 .. outRows - 1]

buildRow :: Image PixelRGB8 -> Int -> Int -> Int -> Vty.Image
buildRow img row outW outH =
  let upperY = row * 2
      lowerY = upperY + 1
      hasLow = lowerY < outH
      cells  = map (\c -> buildCell img c upperY lowerY hasLow) [0 .. outW - 1]
  in  VI.horizCat cells

-- | Build a single terminal cell: one character from the ramp with both
-- foreground and background set to the average color of the 1×2 pixel block.
buildCell :: Image PixelRGB8 -> Int -> Int -> Int -> Bool -> Vty.Image
buildCell img col upperY lowerY hasLow =
  let PixelRGB8 rU gU bU = pixelAt img col upperY
      PixelRGB8 rL gL bL = if hasLow
                           then pixelAt img col lowerY
                           else PixelRGB8 0 0 0
      -- Promote to Int for arithmetic (avoids Word8 overflow on addition)
      ru = fromIntegral rU :: Int
      gu = fromIntegral gU :: Int
      bu = fromIntegral bU :: Int
      rl = fromIntegral rL :: Int
      gl = fromIntegral gL :: Int
      bl = fromIntegral bL :: Int
      -- Average upper + lower → single colour for the whole cell
      avgR = (ru + rl) `div` 2
      avgG = (gu + gl) `div` 2
      avgB = (bu + bl) `div` 2
      -- Luminance: 0.299·R + 0.587·G + 0.114·B
      luma = (avgR * 299 + avgG * 587 + avgB * 114) `div` 1000
      ch   = lumaToChar luma
      clr  = linearColor avgR avgG avgB
      attr = Vty.defAttr `Vty.withForeColor` clr `Vty.withBackColor` clr
  in  VI.char attr ch
