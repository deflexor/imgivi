{-# LANGUAGE NoImplicitPrelude #-}

-- | Keyboard event handling for all keybindings and file operations.
module ImgVi.Events
  ( handleEvent
  , updateDirListing
  ) where

import Relude

import Brick
  ( BrickEvent(..)
  , EventM
  , halt
  )
import qualified Graphics.Vty as V
import qualified System.Directory as Dir
import qualified Data.List as L (partition, zipWith3)
import qualified Data.Text as T

import ImgVi.ImageCache (isImageFile, updateCacheForFile)
import ImgVi.Types (AppState(..), FileItem(..), Name, RenameState(..), RenderMode(..), SelectionMode(..), emptyCache)

-- | Main event handler. Dispatches to rename handler when in rename mode.
handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent vev) = do
  st <- gets id
  case asRename st of
    Just rs -> handleRenameEvent vev rs
    Nothing -> handleNormalEvent vev
handleEvent _ = pass

-- ── Normal mode (no rename active) ─────────────────────

handleNormalEvent :: V.Event -> EventM Name AppState ()
handleNormalEvent vev = case vev of
  V.EvKey (V.KChar 'q') [] -> halt
  V.EvKey V.KEsc []         -> halt

  V.EvKey V.KUp   []        -> moveCursor (-1)
  V.EvKey V.KDown []        -> moveCursor 1

  V.EvKey (V.KChar 's') []  -> toggleSelectCurrent
  V.EvKey (V.KChar ' ') []  -> handleRangeSelect
  V.EvKey (V.KChar 'd') []  -> deleteSelected
  V.EvKey (V.KChar 'r') []  -> startRenameCurrent
  V.EvKey (V.KChar 'm') []  -> cycleRenderMode
  V.EvKey V.KEnter []        -> enterDir

  V.EvResize w h             -> handleResize w h

  _                         -> pass

-- ── Rename mode ────────────────────────────────────────

handleRenameEvent :: V.Event -> RenameState -> EventM Name AppState ()
handleRenameEvent vev rs = case vev of
  -- Cancel
  V.EvKey V.KEsc [] -> cancelRename

  -- Commit
  V.EvKey V.KEnter [] -> commitRename rs

  -- Edit
  V.EvKey V.KBS []     -> renameBackspace rs
  V.EvKey (V.KChar c) [] | c >= ' ' -> renameInsertChar rs c
  V.EvKey V.KDel []    -> renameDelete rs

  -- Navigation
  V.EvKey V.KLeft  [] -> renameMoveCursor rs (-1)
  V.EvKey V.KRight [] -> renameMoveCursor rs 1
  V.EvKey V.KHome []  -> modify $ \s -> s { asRename = Just rs { rsCursor = 0 } }
  V.EvKey V.KEnd  []  -> modify $ \s -> s { asRename = Just rs { rsCursor = T.length (rsBuffer rs) } }

  _ -> pass

-- | Insert a character at the cursor position in the rename buffer.
renameInsertChar :: RenameState -> Char -> EventM Name AppState ()
renameInsertChar rs c = do
  let txt  = rsBuffer rs
      pos  = rsCursor rs
      lhs   = T.take pos txt
      rhs   = T.drop pos txt
      newBuf = lhs <> T.singleton c <> rhs
  modify $ \s -> s { asRename = Just rs { rsBuffer = newBuf, rsCursor = pos + 1 } }

-- | Delete the character before the cursor (Backspace).
renameBackspace :: RenameState -> EventM Name AppState ()
renameBackspace rs = do
  let pos = rsCursor rs
  when (pos > 0) $ do
    let txt  = rsBuffer rs
        lhs   = T.take (pos - 1) txt
        rhs   = T.drop pos txt
    modify $ \s -> s { asRename = Just rs { rsBuffer = lhs <> rhs, rsCursor = pos - 1 } }

-- | Delete the character at the cursor (Delete / Del).
renameDelete :: RenameState -> EventM Name AppState ()
renameDelete rs = do
  let txt  = rsBuffer rs
      pos  = rsCursor rs
  when (pos < T.length txt) $ do
    let lhs = T.take pos txt
        rhs = T.drop (pos + 1) txt
    modify $ \s -> s { asRename = Just rs { rsBuffer = lhs <> rhs } }

-- | Move the rename cursor left or right (clamped to buffer bounds).
renameMoveCursor :: RenameState -> Int -> EventM Name AppState ()
renameMoveCursor rs delta = do
  let newPos = max 0 (min (T.length (rsBuffer rs)) (rsCursor rs + delta))
  modify $ \s -> s { asRename = Just rs { rsCursor = newPos } }

-- | Cancel rename mode, restoring original name.
cancelRename :: EventM Name AppState ()
cancelRename =
  modify $ \s -> s { asRename = Nothing, asStatus = "Rename cancelled" }

-- | Validate and commit the rename.
commitRename :: RenameState -> EventM Name AppState ()
commitRename rs = do
  let newName = rsBuffer rs
  case validateName newName of
    Left err ->
      modify $ \s -> s { asRename = Just rs, asStatus = err }
    Right _ -> do
      st <- gets id
      let oldName = rsOriginal rs
          dir     = asCurrentDir st
          oldPath = dir ++ "/" ++ oldName
          newPath = dir ++ "/" ++ toString newName
      liftIO $ Dir.renameFile oldPath newPath
      modify $ \s -> s { asRename = Nothing, asStatus = "Renamed to " <> newName }
      -- Refresh directory listing
      loadDirIntoState dir

-- | Validate a proposed file name.
validateName :: Text -> Either Text ()
validateName name
  | T.null name                = Left "Error: name cannot be empty"
  | T.any (== '/') name        = Left "Error: name cannot contain '/'"
  | T.any (== '\0') name       = Left "Error: name cannot contain null bytes"
  | T.strip name /= name       = Left "Error: name cannot start or end with whitespace"
  | otherwise                  = Right ()

-- ── Terminal resize ────────────────────────────────────

handleResize :: Int -> Int -> EventM Name AppState ()
handleResize w h = modify $ \s -> s { asTermWidth = w, asTermHeight = h }

-- ── Render mode cycling ─────────────────────────────────

cycleRenderMode :: EventM Name AppState ()
cycleRenderMode = do
  st <- gets id
  let cur = asRenderMode st
      modeNames = ["mono", "fg", "bg", "both"]
      label m = fromMaybe "" (modeNames !!? fromEnum m)
      new = case cur of
              ModeMono -> ModeFg
              ModeFg   -> ModeBg
              ModeBg   -> ModeBoth
              ModeBoth -> ModeMono
  modify $ \s -> s { asRenderMode = new, asStatus = "Render mode: " <> label new }

-- ── File listing helpers (unchanged) ───────────────────

startRenameCurrent :: EventM Name AppState ()
startRenameCurrent = do
  st <- gets id
  case getCurrentItem st of
    Nothing -> modify $ \s -> s { asStatus = "No file selected" }
    Just item -> do
      let originalName = fiPath item
          bufferText   = toText originalName
          cursorPos    = T.length bufferText
      modify $ \s -> s
        { asRename = Just RenameState
            { rsOriginal = originalName
            , rsBuffer   = bufferText
            , rsCursor   = cursorPos
            }
        , asStatus = "Editing name (Enter=confirm, Esc=cancel)"
        }

moveCursor :: Int -> EventM Name AppState ()
moveCursor delta = do
  st       <- gets id
  let files = asFiles st
      cur   = asCursor st
      mx    = max 0 (length files - 1)
      newCursor = max 0 (min mx (cur + delta))
  when (newCursor /= cur) $ do
    modify $ \s -> s { asCursor = newCursor, asStatus = "" }
    dir <- gets asCurrentDir
    when (newCursor < length files) $
      case files !!? newCursor of
        Just item -> loadImageForCursor dir item
        Nothing   -> pass

loadImageForCursor :: FilePath -> FileItem -> EventM Name AppState ()
loadImageForCursor dir item
  | not (isImageFile (fiPath item)) = pass
  | otherwise = do
      cache   <- gets asImgCache
      let fullPath = dir ++ "/" ++ fiPath item
      result <- liftIO $ updateCacheForFile fullPath cache
      case result of
        (newCache, Just _)  -> modify $ \s -> s { asImgCache = newCache }
        (newCache, Nothing) -> modify $ \s -> s { asImgCache = newCache, asStatus = "Failed to decode" }

toggleSelectCurrent :: EventM Name AppState ()
toggleSelectCurrent = do
  st    <- gets id
  let items = asFiles st
      cur   = asCursor st
  when (cur >= 0 && cur < length items) $
    case items !!? cur of
      Just item -> do
        let newSel   = not (fiSelected item)
            newItems = replaceIdx cur (item { fiSelected = newSel }) items
            status   = if newSel
                       then "Selected: " <> fileName (fiPath item)
                       else "Deselected: " <> fileName (fiPath item)
        modify $ \s -> s { asFiles = newItems, asStatus = status }
      Nothing -> pass

handleRangeSelect :: EventM Name AppState ()
handleRangeSelect = do
  st <- gets id
  let files = asFiles st
      cur   = asCursor st
  case asSelMode st of
    Normal ->
      modify $ \s -> s { asSelMode = RangeSelect cur cur }
    RangeSelect start _ ->
      let lo = min start cur
          hi = max start cur
          newItems = zipWith (\idx item ->
            if idx >= lo && idx <= hi
              then item { fiSelected = not (fiSelected item) }
              else item
            ) [0 ..] files
      in  modify $ \s -> s
            { asFiles   = newItems
            , asSelMode = Normal
            , asStatus  = "Toggled range"
            }

deleteSelected :: EventM Name AppState ()
deleteSelected = do
  st <- gets id
  let (selectedItems, _) = L.partition fiSelected (asFiles st)
  case selectedItems of
    []  -> modify $ \s -> s { asStatus = "No files selected" }
    items -> do
      liftIO $ forM_ items $ \item ->
        let fullPath = asCurrentDir st ++ "/" ++ fiPath item
        in  if fiIsDir item
            then Dir.removeDirectoryLink fullPath
            else Dir.removeFile fullPath
      let kept = filter (not . fiSelected) (asFiles st)
      modify $ \s -> s
        { asFiles    = kept
        , asCursor   = min (asCursor st) (max 0 (length kept - 1))
        , asImgCache = emptyCache
        , asStatus   = "Deleted " <> show (length items) <> " file(s)"
        }

enterDir :: EventM Name AppState ()
enterDir = do
  st <- gets id
  case getCurrentItem st of
    Just item | fiIsDir item -> do
      let newDir = asCurrentDir st ++ "/" ++ fiPath item
      normDir <- liftIO $ Dir.makeAbsolute newDir
      loadDirIntoState normDir
    _ -> pass

loadDirIntoState :: FilePath -> EventM Name AppState ()
loadDirIntoState dir = do
  items <- liftIO $ listDirectory dir
  modify $ \s -> s
    { asCurrentDir = dir
    , asFiles      = items
    , asCursor     = 0
    , asSelMode    = Normal
    , asImgCache   = emptyCache
    , asStatus     = ""
    }

getCurrentItem :: AppState -> Maybe FileItem
getCurrentItem st =
  let items = asFiles st
      cur   = asCursor st
  in  items !!? cur

listDirectory :: FilePath -> IO [FileItem]
listDirectory dir = do
  contents <- Dir.listDirectory dir
  let sortedContents = sort contents
      fullPaths = map (\f -> dir ++ "/" ++ f) sortedContents
  isDirs <- mapM Dir.doesDirectoryExist fullPaths
  sizes  <- mapM getFileSize fullPaths
  pure $ L.zipWith3 (\f isDir size -> FileItem f isDir size False) sortedContents isDirs sizes

getFileSize :: FilePath -> IO Integer
getFileSize path = do
  exists <- Dir.doesFileExist path
  if exists
    then Dir.getFileSize path
    else pure 0

replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx _ _ []     = []
replaceIdx 0 x (_:xs) = x : xs
replaceIdx n x (y:ys) = y : replaceIdx (n - 1) x ys

fileName :: FilePath -> Text
fileName = toText . reverse . takeWhile (/= '/') . reverse

updateDirListing :: EventM Name AppState ()
updateDirListing = do
  curDir <- gets asCurrentDir
  loadDirIntoState curDir
  files <- gets asFiles
  whenJust (files !!? 0) $ \item ->
    loadImageForCursor curDir item
