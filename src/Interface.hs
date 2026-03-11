{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

-- | Module interface cache: serialize/deserialize Environment slices for
-- per-module compilation caching. Uses Data.Binary for fast binary encoding.
module Interface
    ( ModuleCache(..)
    , writeModuleCache
    , readModuleCache
    , loadCacheIfFresh
    , cacheVersion
    , toCachePath
    , ensureCacheDir
    , hashSource
    ) where

import State (Environment, sliceEnvironment)
import Surface (Name, ModulePath)

import Data.Binary (Binary(..), encode, decodeOrFail)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Hashable (hash)
import qualified Data.HashMap.Strict as HM
import System.Directory (doesFileExist, createDirectoryIfMissing, getModificationTime)
import System.FilePath ((</>), takeDirectory)
import GHC.Generics (Generic)

-- | Cache format version. Bump this when Environment/CLM/Surface types change
-- to auto-invalidate stale caches.
cacheVersion :: Int
cacheVersion = 10  -- v10: Environment gains targetInstances/targetHandlers/targetExterns, ExternFunc AST node

-- | Cached module: an Environment slice + metadata for freshness checking.
data ModuleCache = ModuleCache
    { mcVersion     :: !Int            -- cache format version
    , mcModuleKey   :: String          -- e.g. "Algebra.Eq"
    , mcSourceHash  :: !Int            -- hash of source .tl file content
    , mcDepsHashes  :: [(String, Int)] -- (depModKey, hash of dep's .tli content)
    , mcEnvironment :: Environment     -- the public slice of this module's env
    } deriving (Show, Generic)

instance Binary ModuleCache

-- | Cache directory (relative to project root)
cacheDir :: FilePath
cacheDir = ".tulam-cache"

-- | Convert a module key (e.g. "Algebra.Eq") to a cache file path
toCachePath :: String -> FilePath
toCachePath modKey = cacheDir </> ("v" ++ show cacheVersion) </> (modKey ++ ".tli")

-- | Ensure the cache directory exists
ensureCacheDir :: IO ()
ensureCacheDir = createDirectoryIfMissing True (cacheDir </> ("v" ++ show cacheVersion))

-- | Hash source text (using Data.Hashable for speed)
hashSource :: T.Text -> Int
hashSource = hash

-- | Write a ModuleCache to disk
writeModuleCache :: ModuleCache -> IO ()
writeModuleCache mc = do
    ensureCacheDir
    let path = toCachePath (mcModuleKey mc)
    BL.writeFile path (encode mc)

-- | Read a ModuleCache from disk. Returns Nothing on decode failure or missing file.
readModuleCache :: String -> IO (Maybe ModuleCache)
readModuleCache modKey = do
    let path = toCachePath modKey
    exists <- doesFileExist path
    if not exists then return Nothing
    else do
        strict <- BS.readFile path
        let bytes = BL.fromStrict strict
        case decodeOrFail bytes of
            Left _ -> return Nothing
            Right (_, _, mc)
                | mcVersion mc /= cacheVersion -> return Nothing
                | otherwise -> return (Just mc)

-- | Load a cached module if the cache is fresh (source hasn't changed
-- and all dependency source hashes still match what was cached).
loadCacheIfFresh :: String -> T.Text -> HM.HashMap String Int -> IO (Maybe ModuleCache)
loadCacheIfFresh modKey sourceText currentDepHashes = do
    mCache <- readModuleCache modKey
    case mCache of
        Nothing -> return Nothing
        Just mc -> do
            let currentHash = hashSource sourceText
            if mcSourceHash mc /= currentHash
                then return Nothing  -- source changed
                else if not (depsMatch (mcDepsHashes mc) currentDepHashes)
                    then return Nothing  -- dependency changed
                    else return (Just mc)
  where
    -- Check that every cached dep hash matches the current hash for that dep
    depsMatch [] _ = True
    depsMatch ((dk, dh):rest) current =
        case HM.lookup dk current of
            Just h  -> h == dh && depsMatch rest current
            Nothing -> False  -- dep not loaded yet = stale cache
