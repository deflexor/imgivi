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
import qualified Data.Text as T
import ImgVi.Types (AppState(..), FileItem(..), Name(..), RenameState(..), SelectionMode(..))

-- | Attribute names for file browser styling.
fileSelectedAttr :: AttrName
fileSelectedAttr = attrName "fileSelected"

fileMarkedAttr :: AttrName
fileMarkedAttr = attrName "fileMarked"

-- | Attribute for the rename edit field (black bg, light red fg).
renameAttr :: AttrName
renameAttr = attrName "renameAttr"

-- | Draw the file browser pane.
drawFileBrowser :: AppState -> Widget Name
drawFileBrowser st =
  viewport FileList Vertical $
  vBox $ zipWith renderItem [0 ..] (asFiles st)
  where
    cursor  = asCursor st
    selMode = asSelMode st
    mRename = asRename st

    renderItem :: Int -> FileItem -> Widget Name
    renderItem idx item =
      let focused   = idx == cursor
          inRange   = case selMode of
                        RangeSelect s e -> idx >= min s e && idx <= max s e
                        Normal          -> False
          isMarked  = fiSelected item
          baseWidget = mkItemWidget item mRename
          styled     = applyStyle focused inRange isMarked baseWidget
      in  if focused then visible styled else styled

    mkItemWidget :: FileItem -> Maybe RenameState -> Widget Name
    mkItemWidget item mr =
      let prefix = if fiIsDir item then " \10095 " else "   "
      in case mr of
           Just rs | rsOriginal rs == fiPath item ->
             -- Render the edit buffer with cursor bar
             let buf     = rsBuffer rs
                 pos     = rsCursor rs
                 lhs     = T.take pos buf
                 rhs     = T.drop pos buf
             in  withAttr renameAttr (txt (prefix <> lhs <> "|" <> rhs))
           _ ->
             txt (prefix <> fileName (fiPath item))

    applyStyle :: Bool -> Bool -> Bool -> Widget Name -> Widget Name
    applyStyle True  _     _     = withAttr fileSelectedAttr
    applyStyle _     True  _     = withAttr fileMarkedAttr
    applyStyle _     _     True  = withAttr fileMarkedAttr
    applyStyle _     _     _     = id

-- | Extract the file name from a path.
fileName :: FilePath -> Text
fileName = toText . reverse . takeWhile (/= '/') . reverse
