import SimpleCmd
import System.IO

program :: String -> IO ()
program image =
  putStrLn ("\n# " ++ image) >>
  cmd_ "skopedate" [image]

images :: [String]
images =
  [ "fedora:39"
  , "centos/centos:stream9"
  , "fedora-toolbox:39"
  , "ubi9"
  , "opensuse/tumbleweed"
  , "docker.io/library/alpine"
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program images
  putStrLn $ "\n" ++ show (length images) ++ " command tests run"
