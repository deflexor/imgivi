{-# LANGUAGE NoImplicitPrelude #-}

-- | Application wiring: Brick App definition, attribute map, and main entry point.
module ImgVi.App
  ( appMain
  ) where

import Relude

import Brick
  ( App(..)
  , AttrMap
  , BrickEvent
  , EventM
  , Widget
  , attrMap
  , attrName
  , customMain
  , getVtyHandle
  , neverShowCursor
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Core as W
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color (ColorMode(..))
import qualified Graphics.Vty.Config as VC
import qualified Graphics.Vty.CrossPlatform as Vc
import qualified Brick.Util as U

import ImgVi.Events (handleEvent, updateDirListing)
import ImgVi.FileBrowser (drawFileBrowser)
import ImgVi.HelpBar (drawHelpBar)
import ImgVi.ImageViewer (drawImageViewer)
import ImgVi.Types (AppState(..), Name(..), initialAppState)

mkAttrMap :: AppState -> AttrMap
mkAttrMap _st =
  attrMap V.defAttr
    [ (attrName "fileSelected", U.on V.white V.cyan)
    , (attrName "fileMarked",   U.on V.white V.yellow)
    , (attrName "helpBar",      U.on V.white V.blue)
    , (attrName "renameAttr",   U.on V.brightRed V.black)
    , (attrName "imgBg",        U.on V.black V.black)
    ]

drawUI :: AppState -> [Widget Name]
drawUI st =
  [ W.vBox
    [ W.hBox
        [ W.hLimit 30 (drawFileBrowser st)
        , B.vBorder
        , drawImageViewer st
        ]
    , W.vLimit 1 (drawHelpBar st)
    ]
  ]

handleAppEvent :: BrickEvent Name () -> EventM Name AppState ()
handleAppEvent = handleEvent

startEvent :: EventM Name AppState ()
startEvent = do
  updateDirListing
  updateTermDims

-- | Query the actual terminal dimensions from vty and store them.
updateTermDims :: EventM Name AppState ()
updateTermDims = do
  vty <- getVtyHandle
  (w, h) <- liftIO $ V.displayBounds (V.outputIface vty)
  modify $ \s -> s { asTermWidth = w, asTermHeight = h }

theApp :: App AppState () Name
theApp = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleAppEvent
  , appStartEvent   = startEvent
  , appAttrMap      = mkAttrMap
  }

-- | Build a 'V.Vty' handle configured for 24-bit TrueColor output.
mkTrueColorVty :: IO V.Vty
mkTrueColorVty = do
  let cfg = VC.defaultConfig { VC.configPreferredColorMode = Just FullColor }
  Vc.mkVty cfg

-- | Main entry point: initialise the terminal and run the app.
appMain :: IO ()
appMain = do
  vty <- mkTrueColorVty
  -- Use sensible defaults; startEvent queries actual dimensions via getVtyHandle
  -- after Brick has fully initialized the terminal (alternate screen, vty context).
  st  <- initialAppState 80 24
  _final <- customMain vty mkTrueColorVty Nothing theApp st
  pure ()
