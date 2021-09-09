{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Extra
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import Data.List
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Network.HTTP.Query (lookupKey)
import SimpleCmd
import System.Environment (getArgs)

imageRegistries :: String -> [String]
imageRegistries image =
  fromMaybe ["docker.io"] $ fmap snd $ listToMaybe $
  filter (\(o,_) -> o `isPrefixOf` image) matchOS
  where
    matchOS :: [(String,[String])]
    matchOS = [("fedora-toolbox",["candidate-registry.fedoraproject.org",
                                  "registry.fedoraproject.org"]),
               ("fedora",["candidate-registry.fedoraproject.org",
                          "registry.fedoraproject.org",
                          "docker.io"]),
               ("centos", ["quay.io", "registry.centos.org","docker.io"]),
               ("ubi", ["registry.access.redhat.com"]),
               ("opensuse", ["registry.opensuse.org", "docker.io"])]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error' "Please specify an image"
    [image] -> checkRegistries image $ imageRegistries image
    _ -> error' "Please only specify an image"

type Image = (UTCTime, Maybe String, String)

checkRegistries :: String -> [String] -> IO ()
checkRegistries image registries = do
  mapM_ skopeoInspectTimeRel registries
  where
    printTime (u,r,s) = do
      t <- utcToLocalZonedTime u
      putStrLn $ show t ++ maybe "" (" rel:" ++) r ++ "  " ++ s

    skopeoInspectTimeRel :: String -> IO ()
    skopeoInspectTimeRel reg = do
      let ref = "docker://" ++ reg ++ "/" ++ image
      mout <- cmdMaybe "skopeo" ["inspect", ref]
      whenJust (maybeParseTimeRel mout) printTime
        where
          maybeParseTimeRel :: Maybe String -> Maybe Image
          maybeParseTimeRel mtxt = do
            obj <- mtxt >>= decode . B.pack
            created <- removeSplitSecs <$> lookupKey "Created" obj
            utc <- parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) created
            labels <- lookupKey "Labels" obj
            return (utc,lookupKey "release" labels,reg)

    -- docker.io has nanosec!
    removeSplitSecs :: String -> String
    removeSplitSecs cs =
      case break (== '.') cs of
        (cs', "") -> cs'
        (cs', _) -> cs' ++ "Z"
