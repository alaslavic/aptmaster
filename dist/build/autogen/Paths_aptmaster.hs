module Paths_aptmaster (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/aptmaster-0.1/ghc-6.12.3"
datadir    = "/usr/local/share/aptmaster-0.1"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "aptmaster_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "aptmaster_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "aptmaster_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "aptmaster_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
