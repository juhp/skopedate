{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (catMaybes)
import Data.List (sortOn)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Data.Tuple.Extra
import Network.HTTP.Query (lookupKey)
import System.Environment (getArgs)

import SimpleCmd


-- FIXME add --pull
main :: IO ()
main = do
  args <- getArgs
  when (null args) $ error' "Please specify an image"
  let image = head args
  -- FIXME be smarter about registry list based on image name
  -- also handle UBI
  -- FIXME make registries configurable
  checkRegistries ["docker.io","registry.fedoraproject.org","candidate-registry.fedoraproject.org","registry.centos.org"] image
--  putStrLn $ show "docker.io" +-+ show (compare t1 t2) +-+ "fedoraproject"

type Image = (UTCTime, String, String)

checkRegistries :: [String] -> String -> IO ()
checkRegistries rs image = do
  results <- catMaybes <$> mapM skopeoInspectTimeRel rs
  mapM_ printTime $ sortOn fst3 results
  where
    printTime (u,r,s) = do
      t <- utcToLocalZonedTime u
      putStrLn $ show t ++ " rel:" ++ r ++ " " ++ s

    skopeoInspectTimeRel :: String -> IO (Maybe Image)
    skopeoInspectTimeRel reg = do
      let ref = "docker://" ++ reg ++ "/" ++ image
      mout <- cmdMaybe "skopeo" ["inspect", ref]
      return $ maybeParseTimeRel (mout)
        where
          maybeParseTimeRel :: Maybe String -> Maybe Image
          maybeParseTimeRel mtxt = do
            obj <- mtxt >>= decode . B.pack
            created <- removeSplitSecs <$> lookupKey "Created" obj
            utc <- parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) created
            labels <- lookupKey "Labels" obj
            rel <- lookupKey "release" labels
            return (utc,rel,reg)

    -- docker.io has nanosec!
    removeSplitSecs :: String -> String
    removeSplitSecs cs =
      case break (== '.') cs of
        (cs', "") -> cs'
        (cs', _) -> cs' ++ "Z"
