{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Data.Aeson
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs)

import Lens.Micro
import Lens.Micro.Aeson

import SimpleCmd


main :: IO ()
main = do
  args <- getArgs
  let image = head args
  checkRegistries ["docker.io","registry.fedoraproject.org","candidate-registry.fedoraproject.org"] image
--  putStrLn $ show "docker.io" +-+ show (compare t1 t2) +-+ "fedoraproject"

type Result = (UTCTime, String)

checkRegistries :: [String] -> String -> IO ()
checkRegistries rs image = do
  results <- catMaybes <$> mapM skopeoInspectTime rs
  mapM_ printTime $ sortBy timeOrder results
  where
    timeOrder (t,_) (t',_) = compare t' t

    printTime (u,s) = do
      t <- utcToLocalZonedTime u
      putStrLn $ show t ++ " " ++ s

    skopeoInspectTime :: String -> IO (Maybe Result)
    skopeoInspectTime reg = do
      let ref = "docker://" ++ reg ++ "/" ++ image
      mout <- cmdMaybe "skopeo" ["inspect", ref] :: IO (Maybe Text)
      return $ maybeParseTime mout
        where
          maybeParseTime :: Maybe Text -> Maybe Result
          maybeParseTime mtxt = do
            txt <- mtxt
            created <- txt ^? key "Created" . _String <&> T.unpack <&> removeSplitSecs
            utc <- parseTimeM False defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%SZ")) created
            return (utc,reg)

    -- docker.io has nanosec!
    removeSplitSecs :: String -> String
    removeSplitSecs cs =
      case break (== '.') cs of
        (cs', "") -> cs'
        (cs', _) -> cs' ++ "Z"
