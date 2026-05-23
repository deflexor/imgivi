{-# LANGUAGE NoImplicitPrelude #-}

-- | File browser widget — left-pane directory listing.
module ImgVi.FileBrowser
  ( drawFileBrowser
  ) where

import Relude

import Brick
  ( AttrName
  , Widget
  , attrName
  , txt
  , vBox
  , ViewportType(..)
  , viewport
  , visible
  , withAttr
  )
import ImgVi.Types (AppState(..), FileItem(..), Name(..), SelectionMode(..))

-- | Attribute names for file browser styling.
fileSelectedAttr :: AttrName
fileSelectedAttr = attrName "fileSelected"

fileMarkedAttr :: AttrName
fileMarkedAttr = attrName "fileMarked"

-- | Draw the file browser pane.
drawFileBrowser :: AppState -> Widget Name
drawFileBrowser st =
  viewport FileList Vertical $
  vBox $ zipWith renderItem [0 ..] (asFiles st)
  where
    cursor  = asCursor st
    selMode = asSelMode st

    renderItem :: Int -> FileItem -> Widget Name
    renderItem idx item =
      let focused   = idx == cursor
          inRange   = case selMode of
                        RangeSelect s e -> idx >= min s e && idx <= max s e
                        Normal          -> False
          isMarked  = fiSelected item
          baseWidget = mkItemWidget item
          styled     = applyStyle focused inRange isMarked baseWidget
      in  if focused then visible styled else styled

    mkItemWidget :: FileItem -> Widget Name
    mkItemWidget item =
      let prefix = if fiIsDir item then " \10095 " else "   "
          name   = fileName (fiPath item)
      in  txt (prefix <> name)

    applyStyle :: Bool -> Bool -> Bool -> Widget Name -> Widget Name
    applyStyle True  _     _     = withAttr fileSelectedAttr
    applyStyle _     True  _     = withAttr fileMarkedAttr
    applyStyle _     _     True  = withAttr fileMarkedAttr
    applyStyle _     _     _     = id

-- | Extract the file name from a path.
fileName :: FilePath -> Text
fileName = toText . reverse . takeWhile (/= '/') . reverse
