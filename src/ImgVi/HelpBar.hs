{-# LANGUAGE NoImplicitPrelude #-}

-- | Bottom help bar showing keybinding reference.
module ImgVi.HelpBar
  ( drawHelpBar
  ) where

import Relude

import Brick
  ( AttrName
  , Widget
  , attrName
  , txt
  , withAttr
  )
import qualified Brick.Widgets.Center as C

import ImgVi.Types (AppState(..), Name)

-- | Attribute for the help bar.
helpBarAttr :: AttrName
helpBarAttr = attrName "helpBar"

-- | Draw the help bar at the bottom of the terminal.
drawHelpBar :: AppState -> Widget Name
drawHelpBar st =
  let status   = asStatus st
      helpText = " [q] Quit  [r] Rename  [d] Delete  [s] Select  [Space] Range-select"
      label    = if status /= "" then status else helpText
  in  withAttr helpBarAttr $
      C.hCenter (txt label)
