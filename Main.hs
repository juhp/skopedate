{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SimpleCmd
-- import Data.Aeson
-- import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)
--import Data.Time.Clock (UTCTime)
--import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime, zonedTimeToUTC)

import Lens.Micro
import Lens.Micro.Aeson


main :: IO ()
main = do
  args <- getArgs
  let image = head args
  t1 <- checkImage "docker.io" image
  t2 <- checkImage "registry.fedoraproject.org" image
  putStrLn $ "docker.io" +-+ show (compare t1 t2) +-+ "fedoraproject"

checkImage :: String -> String -> IO String
checkImage registry image = do
  let ref = registry ++ "/" ++ image
  out <- cmd "skopeo" ["inspect", "docker://" ++ ref] :: IO String
  let created = out ^. key "Created" . _String & T.unpack
--      utc = zonedTimeToUTC created
--  lzt <- utcToLocalZonedTime utc
  putStrLn $ created {- $ show lzt -} +-+ registry
  return created -- utc
