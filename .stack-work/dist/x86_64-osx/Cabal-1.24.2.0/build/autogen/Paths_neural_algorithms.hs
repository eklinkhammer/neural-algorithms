{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_neural_algorithms (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/bin"
libdir     = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/lib/x86_64-osx-ghc-8.0.1/neural-algorithms-0.1.0.0-G4cKiu8m1A6JbAFI6F3DlP"
dynlibdir  = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/lib/x86_64-osx-ghc-8.0.1"
datadir    = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/share/x86_64-osx-ghc-8.0.1/neural-algorithms-0.1.0.0"
libexecdir = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/libexec"
sysconfdir = "/Users/klinkhae/research/haskell_ws/neural-algorithms/.stack-work/install/x86_64-osx/lts-7.18/8.0.1/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "neural_algorithms_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "neural_algorithms_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "neural_algorithms_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "neural_algorithms_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "neural_algorithms_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "neural_algorithms_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
