{-# LANGUAGE NoImplicitPrelude #-}

-- | Core domain types for the imgivi TUI image viewer.
module ImgVi.Types
  ( Name (..)
  , FileItem (..)
  , SelectionMode (..)
  , ImageCache
  , emptyCache
  , lookupCache
  , insertCache
  , AppState (..)
  , initialAppState
  ) where

import Relude
import qualified Data.Map.Strict as Map

import Codec.Picture (DynamicImage)
import System.Directory (getCurrentDirectory)

-- | Widget names used for viewports and click targets.
data Name = FileList | ImageArea
  deriving (Eq, Ord, Show)

-- | Represents a single file or directory entry in the browser.
data FileItem = FileItem
  { fiPath     :: FilePath
  , fiIsDir    :: Bool
  , fiSize     :: Integer
  , fiSelected :: Bool
  }
  deriving (Eq, Ord, Show)

-- | Selection mode for emacs-style range selection.
data SelectionMode
  = Normal
  | RangeSelect Int Int  -- ^ (start, currentEnd)
  deriving (Eq, Ord, Show)

-- | Cache for decoded images, keyed by file path.
-- Stores the raw decoded image; scaling happens at render time.
newtype ImageCache = ImageCache
  { unImageCache :: Map FilePath DynamicImage
  }

-- | Empty image cache.
emptyCache :: ImageCache
emptyCache = ImageCache Map.empty

-- | Look up a cached image by path.
lookupCache :: FilePath -> ImageCache -> Maybe DynamicImage
lookupCache path cache = Map.lookup path (unImageCache cache)

-- | Insert a decoded image into the cache.
insertCache :: FilePath -> DynamicImage -> ImageCache -> ImageCache
insertCache path img cache =
  ImageCache $ Map.insert path img (unImageCache cache)

-- | Main application state.
data AppState = AppState
  { asCurrentDir :: FilePath
  , asFiles      :: [FileItem]
  , asCursor     :: Int
  , asSelMode    :: SelectionMode
  , asImgCache   :: ImageCache
  , asStatus     :: Text
  , asTermWidth  :: Int
  , asTermHeight :: Int
  }

-- | Initial application state for the current directory.
initialAppState :: Int -> Int -> IO AppState
initialAppState termW termH = do
  cwd <- getCurrentDirectory
  pure AppState
    { asCurrentDir = cwd
    , asFiles      = []
    , asCursor     = 0
    , asSelMode    = Normal
    , asImgCache   = emptyCache
    , asStatus     = ""
    , asTermWidth  = termW
    , asTermHeight = termH
    }
