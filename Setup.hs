{-# LANGUAGE LambdaCase #-}

import Control.Monad (when)
import Distribution.Compat.Time (getModTime)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, UserHooks(preBuild))
import Distribution.Simple.Setup (BuildFlags(buildDistPref), fromFlag)
import Distribution.PackageDescription (BuildInfo(hsSourceDirs), emptyBuildInfo)
import System.Directory (createDirectoryIfMissing, findExecutable, doesFileExist)
import System.Process (callProcess)
import System.Exit (die)

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args buildFlags -> do
      capnp <- findExecutable "capnp" >>= \case
        Just path -> return path
        Nothing -> die "setup: Could not find executable capnp"

      let gensrc = fromFlag (buildDistPref buildFlags) ++ "/gensrc"
      createDirectoryIfMissing False gensrc

      let regenerateSchema :: String -> String -> IO ()
          regenerateSchema schema oneOfGeneratedFiles = do
            regenerate <- doesFileExist oneOfGeneratedFiles' >>= \case
              True -> (<) <$> getModTime oneOfGeneratedFiles' <*> getModTime schema
              False -> return True

            when regenerate$
              callProcess capnp ["compile", "-ohaskell:" ++ gensrc, schema]

            where oneOfGeneratedFiles' = gensrc ++ '/' : oneOfGeneratedFiles

      regenerateSchema "schema/Afa.capnp" "Capnp/Gen/Schema/Afa.hs"
      regenerateSchema "schema/CnfAfa.capnp" "Capnp/Gen/Schema/CnfAfa.hs"
      regenerateSchema "schema/SeparatedAfa.capnp" "Capnp/Gen/Schema/SeparatedAfa.hs"

      return (Just emptyBuildInfo{hsSourceDirs=[gensrc]}, [])
  }
