module Paths_lambdalumps (
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

bindir     = "/home/solvi/bin"
libdir     = "/home/solvi/lib/x86_64-linux-ghc-7.10.3/lambdalumps-0.1.0.0-EA2vwAzCZ1aEJed0DYGnHk"
datadir    = "/home/solvi/share/x86_64-linux-ghc-7.10.3/lambdalumps-0.1.0.0"
libexecdir = "/home/solvi/libexec"
sysconfdir = "/home/solvi/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lambdalumps_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lambdalumps_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "lambdalumps_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lambdalumps_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lambdalumps_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
