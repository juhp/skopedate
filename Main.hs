{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SimpleCmd
-- import Data.Aeson
import Data.List (sortBy)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)

import Lens.Micro
import Lens.Micro.Aeson


main :: IO ()
main = do
  args <- getArgs
  let image = head args
  checkRegistries ["docker.io","registry.fedoraproject.org","candidate-registry.fedoraproject.org"] image
--  putStrLn $ show "docker.io" +-+ show (compare t1 t2) +-+ "fedoraproject"

checkRegistries :: [String] -> String -> IO ()
checkRegistries rs image = do
  let refs = map (\ r -> "docker://" ++ r ++ "/" ++ image) rs
  times <- mapM skopeoInspectTime refs
--      utc = zonedTimeToUTC created
--  lzt <- utcToLocalZonedTime utc
  mapM_ printTime $ sortBy timeOrder $ zip times rs
--  return created -- utc
  where
    timeOrder (t,_) (t',_) = compare t' t

    printTime (u,s) = do
      t <- utcToLocalZonedTime u
      putStrLn $ show t ++ " " ++ s

skopeoInspectTime :: String -> IO UTCTime
skopeoInspectTime ref = do
  out <- cmd "skopeo" ["inspect", ref] :: IO Text
  let created = out ^. key "Created" . _String & T.unpack & removeSplitSecs
  return $ parseTimeOrError False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) created
  where
    -- docker.io has nanosec!
    removeSplitSecs :: String -> String
    removeSplitSecs cs =
      case break (== '.') cs of
        (cs', "") -> cs'
        (cs', _) -> cs' ++ "Z"
