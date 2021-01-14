module Main where

import MakeLabel
import McMC
import TransformItem

prefix :: FilePath
prefix = "/Users/ppelleti/misc/"

htmlfile :: FilePath
htmlfile = prefix ++ "0104ppelleti.html"

destDir :: FilePath
destDir = "/tmp/labels/"

writeLabel :: LineItem -> IO ()
writeLabel item = do
  let destFile = destDir ++ liPoNo item ++ "-" ++ liLineNo item ++ ".label"
  makeLabel destFile item

main :: IO ()
main = do
  items <- lineItemsFromFile htmlfile
  items' <- transformItems prefix items
  mapM_ writeLabel items'
