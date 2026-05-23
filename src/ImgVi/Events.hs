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

import ImgVi.ImageCache (isImageFile, updateCacheForFile)
import ImgVi.Types (AppState(..), FileItem(..), Name, SelectionMode(..), emptyCache)

-- | Main event handler.
handleEvent :: BrickEvent Name () -> EventM Name AppState ()
handleEvent (VtyEvent vev) = case vev of
  V.EvKey (V.KChar 'q') [] -> halt
  V.EvKey V.KEsc []         -> halt

  V.EvKey V.KUp   []        -> moveCursor (-1)
  V.EvKey V.KDown []        -> moveCursor 1

  V.EvKey (V.KChar 's') []  -> toggleSelectCurrent
  V.EvKey (V.KChar ' ') []  -> handleRangeSelect
  V.EvKey (V.KChar 'd') []  -> deleteSelected
  V.EvKey (V.KChar 'r') []  -> renameCurrent
  V.EvKey V.KEnter []        -> enterDir

  V.EvResize w h             -> handleResize w h

  _                         -> pass
handleEvent _ = pass

-- | Update stored terminal dimensions on resize.
handleResize :: Int -> Int -> EventM Name AppState ()
handleResize w h = modify $ \s -> s { asTermWidth = w, asTermHeight = h }

-- | Move the cursor up/down and trigger image cache update.
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

-- | Load (or refresh cache for) the image at the cursor.
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

-- | Toggle selection of the current file.
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

-- | Handle Space key for emacs-style range selection.
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
          sel = case files !!? cur of
                  Just item -> not (fiSelected item)
                  Nothing   -> False
          newItems = zipWith (\idx item ->
            if idx >= lo && idx <= hi
              then item { fiSelected = sel }
              else item
            ) [0 ..] files
      in  modify $ \s -> s
            { asFiles   = newItems
            , asSelMode = Normal
            , asStatus  = "Selected range"
            }

-- | Delete selected file(s).
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

-- | Rename current file.
renameCurrent :: EventM Name AppState ()
renameCurrent = do
  st <- gets id
  case getCurrentItem st of
    Nothing -> modify $ \s -> s { asStatus = "No file selected" }
    Just item -> do
      liftIO $ do
        putStr "New name: "
        newName <- getLine
        let oldPath = asCurrentDir st ++ "/" ++ fiPath item
            newPath = asCurrentDir st ++ "/" ++ toString newName
        Dir.renameFile oldPath newPath
      loadDirIntoState (asCurrentDir st)

-- | Enter the selected directory.
enterDir :: EventM Name AppState ()
enterDir = do
  st <- gets id
  case getCurrentItem st of
    Just item | fiIsDir item -> do
      let newDir = asCurrentDir st ++ "/" ++ fiPath item
      normDir <- liftIO $ Dir.makeAbsolute newDir
      loadDirIntoState normDir
    _ -> pass

-- | Refresh directory listing into state.
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

-- | Get the current file item (safe).
getCurrentItem :: AppState -> Maybe FileItem
getCurrentItem st =
  let items = asFiles st
      cur   = asCursor st
  in  items !!? cur

-- | List directory contents, returning FileItems.
listDirectory :: FilePath -> IO [FileItem]
listDirectory dir = do
  contents <- Dir.listDirectory dir
  let sortedContents = sort contents
      fullPaths = map (\f -> dir ++ "/" ++ f) sortedContents
  isDirs <- mapM Dir.doesDirectoryExist fullPaths
  sizes  <- mapM getFileSize fullPaths
  pure $ L.zipWith3 (\f isDir size -> FileItem f isDir size False) sortedContents isDirs sizes

-- | Get file size (0 for directories or errors).
getFileSize :: FilePath -> IO Integer
getFileSize path = do
  exists <- Dir.doesFileExist path
  if exists
    then Dir.getFileSize path
    else pure 0

-- | Replace element at index in list.
replaceIdx :: Int -> a -> [a] -> [a]
replaceIdx _ _ []     = []
replaceIdx 0 x (_:xs) = x : xs
replaceIdx n x (y:ys) = y : replaceIdx (n - 1) x ys

-- | Extract file name (last component) from a path.
fileName :: FilePath -> Text
fileName = toText . reverse . takeWhile (/= '/') . reverse

-- | Update the directory listing (called on startup).
updateDirListing :: EventM Name AppState ()
updateDirListing = do
  curDir <- gets asCurrentDir
  loadDirIntoState curDir
  files <- gets asFiles
  whenJust (files !!? 0) $ \item ->
    loadImageForCursor curDir item
