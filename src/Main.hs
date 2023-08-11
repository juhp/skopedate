{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Extra (when, whenJust)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Network.HTTP.Query (lookupKey)
import SimpleCmd (needProgram)
import SimpleCmdArgs (simpleCmdArgs, switchWith, strArg)
import System.Process.Typed (proc, readProcessStdout, ExitCode(ExitSuccess))

import Paths_skopedate

imageRegistries :: String -> [String]
imageRegistries image =
  fromMaybe ["docker.io"] $ lookup image matchOS
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
  mapM_ skopeoInspectTimeRel $ imageRegistries image
  where
    skopeoInspectTimeRel :: String -> IO ()
    skopeoInspectTimeRel reg = do
      let ref = "docker://" ++ reg ++ "/" ++ image
      (res,out) <- readProcessStdout $ proc "skopeo" ["inspect", ref]
      when (res == ExitSuccess) $ do
        when debug $ B.putStrLn out
        whenJust (parseTimeRel out) printTime
        where
          parseTimeRel :: B.ByteString -> Maybe Image
          parseTimeRel bs = do
            obj <- decode bs
            created <- removeSplitSecs <$> lookupKey "Created" obj
            utc <- parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) created
            labels <- lookupKey "Labels" obj
            return (utc,lookupKey "release" labels,reg)

          printTime (u,mr,s) = do
            t <- utcToLocalZonedTime u
            putStrLn $ show t ++ maybeRel mr ++ "  " ++ s

          maybeRel :: Maybe String -> String
          maybeRel Nothing = ""
          maybeRel (Just r) =
            " rel:" ++ (if length r < 2 then " " else "") ++ r

    -- docker.io has nanosec!
    removeSplitSecs :: String -> String
    removeSplitSecs cs =
      case break (== '.') cs of
        (cs', "") -> cs'
        (cs', _) -> cs' ++ "Z"
