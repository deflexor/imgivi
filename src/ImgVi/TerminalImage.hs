{-# LANGUAGE NoImplicitPrelude #-}

-- | Render a 'DynamicImage' as a vty 'Image' using the img2ascii algorithm.
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

import ImgVi.Types (RenderMode(..))

-- | Character ramp: dense (dark) → sparse (bright). Same as img2ascii default.
ramp :: String
ramp = "@#8&|o:*_. "

-- | Map average luminance (0–255) to a character from the ramp.
lumaToChar :: Int -> Char
lumaToChar luma =
  let rampLen = length ramp - 1
      idx     = ceiling (fromIntegral (rampLen * luma) / 255 :: Double)
  in  fromMaybe (fromMaybe ' ' (reverse ramp !!? 0)) (drop idx ramp !!? 0)

-- | Render a dynamic image to fit within the given terminal cell dimensions,
-- preserving aspect ratio, using the requested colour mode.
renderImage :: DynamicImage -> Int -> Int -> RenderMode -> Vty.Image
renderImage dynImg paneCols paneRows renderMode
  | paneCols <= 0 || paneRows <= 0 = VI.emptyImage
  | otherwise =
    let img     = convertRGB8 dynImg
        srcW    = imageWidth img
        srcH    = imageHeight img
        maxPxW  = paneCols
        maxPxH  = paneRows * 2
        scaleF  = min (fromIntegral maxPxW / fromIntegral srcW :: Double)
                      (fromIntegral maxPxH / fromIntegral srcH :: Double)
        outW    = max 1 (round (fromIntegral srcW * scaleF :: Double))
        outH    = max 1 (round (fromIntegral srcH * scaleF :: Double))
        outRows = (outH + 1) `div` 2
        scaled  = scaleBilinear outW outH img
    in  VI.vertCat $ map (\r -> buildRow scaled r outW outH renderMode) [0 .. outRows - 1]

buildRow :: Image PixelRGB8 -> Int -> Int -> Int -> RenderMode -> Vty.Image
buildRow img row outW outH renderMode =
  let upperY = row * 2
      lowerY = upperY + 1
      hasLow = lowerY < outH
      cells  = map (\c -> buildCell img c upperY lowerY hasLow renderMode) [0 .. outW - 1]
  in  VI.horizCat cells

-- | Build a single terminal cell: one character from the ramp with colour
-- configured according to the 'RenderMode'.
buildCell :: Image PixelRGB8 -> Int -> Int -> Int -> Bool -> RenderMode -> Vty.Image
buildCell img col upperY lowerY hasLow renderMode =
  let PixelRGB8 rU gU bU = pixelAt img col upperY
      PixelRGB8 rL gL bL = if hasLow
                           then pixelAt img col lowerY
                           else PixelRGB8 0 0 0
      ru = fromIntegral rU :: Int
      gu = fromIntegral gU :: Int
      bu = fromIntegral bU :: Int
      rl = fromIntegral rL :: Int
      gl = fromIntegral gL :: Int
      bl = fromIntegral bL :: Int
      avgR = (ru + rl) `div` 2
      avgG = (gu + gl) `div` 2
      avgB = (bu + bl) `div` 2
      luma = (avgR * 299 + avgG * 587 + avgB * 114) `div` 1000
      ch   = lumaToChar luma
      clr  = linearColor avgR avgG avgB
      attr = case renderMode of
               ModeMono -> Vty.defAttr
               ModeFg   -> Vty.defAttr `Vty.withForeColor` clr
               ModeBg   -> Vty.defAttr `Vty.withBackColor` clr
               ModeBoth -> Vty.defAttr `Vty.withForeColor` clr `Vty.withBackColor` clr
  in  VI.char attr ch
