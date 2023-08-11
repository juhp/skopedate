{-# LANGUAGE CPP, OverloadedStrings #-}

module Main (main) where

import Control.Monad.Extra (unless, when)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (breakOn, isPrefixOf)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.HTTP.Query (lookupKey, (+/+))
import SimpleCmd (needProgram, warning)
import SimpleCmdArgs (simpleCmdArgs, switchWith, strArg)
import System.Console.ANSI (clearFromCursorToLineBeginning)
import System.IO (hFlush, hPutChar, hPutStr, stderr)
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
  simpleCmdArgs (Just version) "Checks dates of latest container images"
    "A tool for seeing the dates of latest container images in registries" $
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
    [] -> skopeoInspectTimeRel "" image
    regs -> mapM_ (skopeoInspectTimeRel image) regs
  where
    skopeoInspectTimeRel :: String -> String -> IO ()
    skopeoInspectTimeRel img reg = do
      let ref = "docker://" ++ reg +/+ img
      hPutStr stderr $ if debug then ref else ' ' : reg
      hPutChar stderr ' '
      hFlush stderr
      (ok,out) <- readProcessStdout $ proc "skopeo" ["inspect", ref]
      when (ok == ExitSuccess) $ do
        when debug $ do
          clearFromCursorToLineBeginning
          hPutChar stderr '\r'
          B.putStrLn out
        case parseTimeRel out of
          Just res -> do
            unless debug $ do
              clearFromCursorToLineBeginning
              hPutChar stderr '\r'
            printTime res
          Nothing -> warning " image not found"
        where
          parseTimeRel :: B.ByteString -> Maybe (UTCTime, Maybe String, String)
          parseTimeRel bs = do
            obj <- decode bs
            utc <- lookupKey "Created" obj
            labels <- lookupKey "Labels" obj
            return (utc,lookupKey "release" labels,reg)

          printTime (u,mr,s) = do
            t <- utcToLocalZonedTime u
            putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" t ++ maybeRel mr ++ "  " ++ s

          maybeRel :: Maybe String -> String
          maybeRel Nothing = ""
          maybeRel (Just r) =
            " rel:" ++ (if length r < 2 then " " else "") ++ r
