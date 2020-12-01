module Main (main) where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  dir <- getCurrentDirectory
  let path = dir </> "Setup.hs"
  writeFile path $
    unlines
      [ "import Distribution.Simple",
        "main = defaultMain"
      ]
  putStrLn $ "Write file: " <> path