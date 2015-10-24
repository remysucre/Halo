module Paths_Halo (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/remy/Halo/.cabal-sandbox/bin"
libdir     = "/home/remy/Halo/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.2/Halo-0.1.0.0-3rZjU0wZDJA8wsUQHvxTOa"
datadir    = "/home/remy/Halo/.cabal-sandbox/share/x86_64-linux-ghc-7.10.2/Halo-0.1.0.0"
libexecdir = "/home/remy/Halo/.cabal-sandbox/libexec"
sysconfdir = "/home/remy/Halo/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Halo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Halo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Halo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Halo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Halo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
