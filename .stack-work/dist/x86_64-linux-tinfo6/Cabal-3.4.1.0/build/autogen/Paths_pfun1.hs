{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_pfun1 (
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

bindir     = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/bin"
libdir     = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/lib/x86_64-linux-ghc-9.0.2/pfun1-0.1.0.0-6LgWVxlUg0GATq02kU8MyO"
dynlibdir  = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/lib/x86_64-linux-ghc-9.0.2"
datadir    = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/share/x86_64-linux-ghc-9.0.2/pfun1-0.1.0.0"
libexecdir = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/libexec/x86_64-linux-ghc-9.0.2/pfun1-0.1.0.0"
sysconfdir = "/home/joachim/Documents/github/phd/pfun1/.stack-work/install/x86_64-linux-tinfo6/c34e084d07a4bc7ceb4c11c4d2dba3e900480d5359bbb22d237198d87afe4781/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pfun1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pfun1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pfun1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pfun1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pfun1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pfun1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
