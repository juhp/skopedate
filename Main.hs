{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import SimpleCmd
import Data.Aeson
import Data.Char (toUpper)
-- import Data.Text (Text)
--import qualified Data.Text.IO as T
import GHC.Generics
import System.Environment (getArgs)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (ZonedTime, utcToLocalZonedTime, zonedTimeToUTC)

data Image =
  Image { -- name  :: !Text
--        , digest :: !Text
--        , repoTags :: [Text],
          created :: !ZonedTime
--        , dockerVersion :: !Text
--        , labels :: !Object
--        , architecture :: !Text
--        , os :: !Text
--        , layers :: [Text]
        } deriving (Show, Generic)

customOptions :: Options
customOptions = defaultOptions
                { fieldLabelModifier = \ (c:cs) -> toUpper c : cs
                }

instance FromJSON Image where
    parseJSON = genericParseJSON customOptions

main :: IO ()
main = do
  args <- getArgs
  let image = head args
  t1 <- checkImage "docker.io" image
  t2 <- checkImage "registry.fedoraproject.org" image
  putStrLn $ "docker.io" +-+ (show $ compare t1 t2) +-+ "fedoraproject"
    

checkImage :: String -> String -> IO UTCTime
checkImage registry image = do
  let ref = registry ++ "/" ++ image
  out <- cmd "skopeo" ["inspect", "docker://" ++ ref]
  let res = eitherDecode out
  case res of
    Left err -> error err
    Right img -> do
      let utc = zonedTimeToUTC $ created img
      lzt <- utcToLocalZonedTime utc
      putStrLn $ show lzt +-+ ref
      return utc
