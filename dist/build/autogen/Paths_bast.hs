module Paths_bast (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/schell/Library/Haskell/ghc-7.6.3/lib/bast-0.1/bin"
libdir     = "/Users/schell/Library/Haskell/ghc-7.6.3/lib/bast-0.1/lib"
datadir    = "/Users/schell/Library/Haskell/ghc-7.6.3/lib/bast-0.1/share"
libexecdir = "/Users/schell/Library/Haskell/ghc-7.6.3/lib/bast-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "bast_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bast_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bast_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bast_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
