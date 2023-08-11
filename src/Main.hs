{-# LANGUAGE CPP, OverloadedStrings #-}

module Main (main) where

import Control.Monad.Extra (when)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (breakOn, isPrefixOf)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.HTTP.Query (lookupKey, (+/+))
import SimpleCmd (needProgram, warning)
import SimpleCmdArgs (simpleCmdArgs, switchWith, strArg)
import System.IO (hFlush, stdout)
import System.Process.Typed (proc, readProcessStdout,
#if MIN_VERSION_typed_process(0,2,8)
                             ExitCode(ExitSuccess)
#endif
                            )
#if !MIN_VERSION_typed_process(0,2,8)
import System.Exit (ExitCode(ExitSuccess))
#endif

import Paths_skopedate

main :: IO ()
main = do
  simpleCmdArgs (Just version)
    "A tool for checking dates of registry container images"
    "See https://github.com/juhp/skopedate#readme" $
    checkRegistries
    <$> switchWith 'd' "debug" "show debug output"
    <*> strArg "IMAGE"

imageRegistries :: String -> [String]
imageRegistries image =
  let (untagged,_tag) = breakOn ":" image
  in
    case filter (\(i,_) -> i `isPrefixOf` untagged) matchOS of
      (_,rs):_ -> rs
      [] ->
        case breakOn "/" untagged of
          (_, "") -> ["docker.io"]
          (before,_after) ->
            ["docker.io" | '.' `notElem` before]
  where
    matchOS :: [(String,[String])]
    matchOS = [("fedora-toolbox",["registry.fedoraproject.org",
                                  "candidate-registry.fedoraproject.org"]),
               ("fedora",["registry.fedoraproject.org",
                          "candidate-registry.fedoraproject.org",
                          "quay.io/fedora",
                          "docker.io"]),
               ("centos", ["quay.io"]),
               ("ubi", ["registry.access.redhat.com"]),
               ("opensuse", ["registry.opensuse.org", "docker.io"])]

checkRegistries :: Bool -> String -> IO ()
checkRegistries debug image = do
  needProgram "skopeo"
  case imageRegistries image of
    [] -> skopeoInspectTimeRel "" (length image) image
    regs ->
      let width = maximum $ map length regs
      in mapM_ (skopeoInspectTimeRel image width) regs
  where
    skopeoInspectTimeRel :: String -> Int -> String -> IO ()
    skopeoInspectTimeRel img width reg = do
      let ref = "docker://" ++ reg +/+ img
      putStr $ if debug
               then ref
               else reg ++ replicate (width - length reg + 1) ' '
      hFlush stdout
      (ok,out) <- readProcessStdout $ proc "skopeo" ["inspect", ref]
      when (ok == ExitSuccess) $ do
        when debug $ do
          putChar '\n'
          B.putStrLn out
        case parseTimeRel out of
          Just res -> printTime res
          Nothing -> warning "image not found"
        where
          parseTimeRel :: B.ByteString -> Maybe (UTCTime, Maybe String)
          parseTimeRel bs = do
            obj <- decode bs
            utc <- lookupKey "Created" obj
            let mlabels = lookupKey "Labels" obj
            return (utc, mlabels >>= lookupKey "release")

          printTime (u,mr) = do
            t <- utcToLocalZonedTime u
            putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" t ++ maybeRel mr

          maybeRel :: Maybe String -> String
          maybeRel Nothing = ""
          maybeRel (Just r) =
            " rel:" ++ (if length r < 2 then " " else "") ++ r
