{-# LANGUAGE NoImplicitPrelude #-}

-- | Image loading, decoding, and caching.
module ImgVi.ImageCache
  ( loadImage
  , updateCacheForFile
  , isImageFile
  ) where

import Relude

import Codec.Picture
  ( DynamicImage
  , decodeImage
  )

import qualified Data.ByteString as BS
import qualified Data.Text as T

import ImgVi.Types (ImageCache, insertCache, lookupCache)

-- | Try to load and decode an image file.
loadImage :: FilePath -> IO (Either Text DynamicImage)
loadImage path = do
  bs <- BS.readFile path
  pure $ case decodeImage bs of
    Right img -> Right img
    Left  msg -> Left (toText msg)

-- | Load and cache a raw decoded image for the given path.
-- Scaling is deferred to render time so the same cache entry
-- works at any terminal size.
updateCacheForFile
  :: FilePath -> ImageCache -> IO (ImageCache, Maybe DynamicImage)
updateCacheForFile path cache =
  case lookupCache path cache of
    Just cached -> pure (cache, Just cached)
    Nothing     -> do
      result <- loadImage path
      case result of
        Left _err -> pure (cache, Nothing)
        Right dynImg ->
          pure (insertCache path dynImg cache, Just dynImg)

-- | Common image file extensions (lowercase).
imageExtensions :: [Text]
imageExtensions = [".png", ".jpg", ".jpeg", ".gif", ".bmp", ".tiff", ".tif", ".webp"]

-- | Check if a file path has a common image extension (case-insensitive).
isImageFile :: FilePath -> Bool
isImageFile path =
  let ext = toTextLower (toText (takeExt path))
  in  ext `elem` imageExtensions

-- | Extract the extension from a 'FilePath' (including the dot).
-- Example: takeExt "tst.png" -> ".png"
takeExt :: FilePath -> FilePath
takeExt fp =
  let rev = reverse fp
      (extRev, rest) = break (== '.') rev
  in  case rest of
        '.' : _ -> "." <> reverse extRev
        _       -> ""

-- | Lowercase a text value.
toTextLower :: Text -> Text
toTextLower = T.toLower
