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
              callProcess capnp
                ["compile", "-ohaskell:" ++ gensrc, "--src-prefix=schema", schema]

            where oneOfGeneratedFiles' = gensrc ++ '/' : oneOfGeneratedFiles

      regenerateSchema "schema/Afa/Model/Term.capnp" "Capnp/Gen/Afa/Model/Term.hs"
      regenerateSchema "schema/Afa/Model/Succinct.capnp" "Capnp/Gen/Afa/Model/Succinct.hs"
      regenerateSchema "schema/Afa/Model/CnfAfa.capnp" "Capnp/Gen/Afa/Model/CnfAfa.hs"
      regenerateSchema "schema/Afa/Model/Separated.capnp" "Capnp/Gen/Afa/Model/SeparatedAfa.hs"
      regenerateSchema "schema/Afa/Rpc/ModelChecker.capnp" "Capnp/Gen/Afa/Rpc/ModelChecker.hs"
      regenerateSchema "schema/Afa/Rpc/ModelCheckers.capnp" "Capnp/Gen/Afa/Rpc/ModelCheckers.hs"

      return (Just emptyBuildInfo{hsSourceDirs=[gensrc]}, [])
  }
