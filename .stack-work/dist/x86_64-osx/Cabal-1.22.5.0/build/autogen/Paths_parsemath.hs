module Paths_parsemath (
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

bindir     = "/Users/dangitstam/Documents/parsemath/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/bin"
libdir     = "/Users/dangitstam/Documents/parsemath/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/lib/x86_64-osx-ghc-7.10.3/parsemath-0.1.0.0-2FlZuVjNcM648ms5omIPPi"
datadir    = "/Users/dangitstam/Documents/parsemath/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/share/x86_64-osx-ghc-7.10.3/parsemath-0.1.0.0"
libexecdir = "/Users/dangitstam/Documents/parsemath/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/libexec"
sysconfdir = "/Users/dangitstam/Documents/parsemath/.stack-work/install/x86_64-osx/lts-6.10/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "parsemath_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parsemath_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "parsemath_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parsemath_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "parsemath_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
