module Main where

import Text.Printf
import Text.Read

import MakeLabel
import McMC
import TransformItem

prefix :: FilePath
prefix = "/Users/ppelleti/misc/"

htmlfile :: FilePath
htmlfile = prefix ++ "0104ppelleti.html"

destDir :: FilePath
destDir = "/Users/ppelleti/Documents/DYMO Label Software/Labels/"

fmtInt :: Int -> String
fmtInt n = printf "%03d" n

zeroPad :: String -> String
zeroPad lineNo =
  case readMaybe lineNo of
    Nothing -> lineNo
    Just n -> fmtInt n

writeLabel :: LineItem -> IO ()
writeLabel item = do
  let lineNo = zeroPad (liLineNo item)
  let destFile = destDir ++ liPoNo item ++ "-" ++ lineNo ++ ".label"
  makeLabel destFile item

main :: IO ()
main = do
  items <- lineItemsFromFile htmlfile
  items' <- transformItems prefix items
  mapM_ writeLabel items'
