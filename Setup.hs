{-# LANGUAGE LambdaCase #-}

import Control.Monad (forM)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(preBuild))
import Distribution.Simple.Setup (BuildFlags(buildDistPref), fromFlag)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), emptyBuildInfo)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Process (callProcess)
import System.Exit (die)

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args buildFlags -> do
      [capnp, _] <- forM ["capnp", "capnpc-haskell"]$ \exe ->
        findExecutable exe >>= \case
          Just path -> return path
          Nothing -> die$ "setup: Could not find executable " ++ exe

      let gensrc = fromFlag (buildDistPref buildFlags) ++ "/gensrc"
      createDirectoryIfMissing False gensrc
      callProcess capnp ["compile", "-ohaskell:" ++ gensrc, "schema/afa.capnp"]

      return (Just emptyBuildInfo{hsSourceDirs=[gensrc]}, [])
  }