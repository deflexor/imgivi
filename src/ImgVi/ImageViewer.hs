{-# LANGUAGE NoImplicitPrelude #-}

-- | Image viewer widget — right-pane image preview.
module ImgVi.ImageViewer
  ( drawImageViewer
  ) where

import Relude

import Brick
  ( Widget
  , raw
  , txt
  )
import qualified Brick.Widgets.Center as C

import ImgVi.TerminalImage (renderImage)
import ImgVi.Types (AppState(..), FileItem(..), Name(..), lookupCache)
import ImgVi.ImageCache (isImageFile)

-- | Draw the image viewer pane, scaling the image to fill available space.
drawImageViewer :: AppState -> Widget Name
drawImageViewer st =
  case findFileAtCursor st of
    Nothing       -> C.center (txt "No files in directory")
    Just item
      | not (isImageFile (fiPath item)) -> C.center (txt "Not an image file")
      | otherwise ->
          let cacheKey = asCurrentDir st ++ "/" ++ fiPath item
              -- Terminal dimensions (corrected by getVtyHandle in startEvent);
              -- fall back to safe defaults if not yet set.
              tw   = if asTermWidth st > 0 then asTermWidth st else 80
              th   = if asTermHeight st > 0 then asTermHeight st else 24
              -- File browser takes 30 columns, border takes 1
              paneW = max 1 (tw - 30 - 1)
              -- Full height minus help bar row
              paneH = max 1 (th - 2)
          in  case lookupCache cacheKey (asImgCache st) of
                Nothing    -> C.center (txt "Loading...")
                Just dynImg ->
                  C.center (raw (renderImage dynImg paneW paneH (asRenderMode st)))

-- | Find the file at the current cursor position.
findFileAtCursor :: AppState -> Maybe FileItem
findFileAtCursor st =
  case drop (asCursor st) (asFiles st) of
    (x:_) -> Just x
    []    -> Nothing
