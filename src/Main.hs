{-# LANGUAGE CPP, OverloadedStrings #-}

module Main (main) where

import Control.Monad.Extra (when, whenJust)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Extra (dropWhileEnd)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.HTTP.Query (lookupKey, (+/+))
import SimpleCmd (needProgram)
import SimpleCmdArgs (simpleCmdArgs, switchWith, strArg)
import System.Process.Typed (proc, readProcessStdout,
#if MIN_VERSION_typed_process(0,2,8)
                             ExitCode(ExitSuccess)
#endif
                            )
#if !MIN_VERSION_typed_process(0,2,8)
import System.Exit (ExitCode(ExitSuccess))
#endif

import Paths_skopedate

imageRegistries :: String -> [String]
imageRegistries image =
  case lookup image matchOS of
    Just rs -> rs
    Nothing ->
      if '/' `elem`  image
      then
        case dropWhileEnd (/= '/') image of
          "" -> ["docker.io"]
          loc ->
            if '.' `elem` loc
            then []
            else ["docker.io"]
      else ["docker.io"]
  where
    matchOS :: [(String,[String])]
    matchOS = [("fedora-toolbox",["candidate-registry.fedoraproject.org",
                                  "registry.fedoraproject.org"]),
               ("fedora",["candidate-registry.fedoraproject.org",
                          "registry.fedoraproject.org",
                          "docker.io",
                          "quay.io/fedora"]),
               ("centos", ["quay.io", "registry.centos.org","docker.io"]),
               ("ubi", ["registry.access.redhat.com"]),
               ("opensuse", ["registry.opensuse.org", "docker.io"])]

main :: IO ()
main = do
  needProgram "skopeo"
  simpleCmdArgs (Just version) "Check dates of latest container images"
    "description" $
    checkRegistries
    <$> switchWith 'd' "debug" "show json output"
    <*> strArg "IMAGE"

type Image = (UTCTime, Maybe String, String)

checkRegistries :: Bool -> String -> IO ()
checkRegistries debug image = do
  case imageRegistries image of
    [] -> skopeoInspectTimeRel "" image
    regs -> mapM_ (skopeoInspectTimeRel image) regs
  where
    skopeoInspectTimeRel :: String -> String -> IO ()
    skopeoInspectTimeRel img reg = do
      let ref = "docker://" ++ reg +/+ img
      print ref
      (res,out) <- readProcessStdout $ proc "skopeo" ["inspect", ref]
      when (res == ExitSuccess) $ do
        when debug $ B.putStrLn out
        whenJust (parseTimeRel out) printTime
        where
          parseTimeRel :: B.ByteString -> Maybe Image
          parseTimeRel bs = do
            obj <- decode bs
            utc <- lookupKey "Created" obj
            labels <- lookupKey "Labels" obj
            return (utc,lookupKey "release" labels,reg)

          printTime (u,mr,s) = do
            t <- utcToLocalZonedTime u
            putStrLn $ removeSplitSecs (show t) ++ maybeRel mr ++ "  " ++ s

          removeSplitSecs :: String -> String
          removeSplitSecs cs =
            case break (== '.') cs of
              (cs', "") -> cs'
              (cs', _) -> cs' ++ "Z"

          maybeRel :: Maybe String -> String
          maybeRel Nothing = ""
          maybeRel (Just r) =
            " rel:" ++ (if length r < 2 then " " else "") ++ r
