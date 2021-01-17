module Main where

import Data.List
import System.Environment
import Text.Printf
import Text.Read

import MakeLabel
import McMC
import McOptions
import TransformItem

htmlfile :: FilePath
htmlfile = "/Users/ppelleti/orders/McMaster-Carr/0112ppelleti.html"

getLabelDir :: IO FilePath
getLabelDir = do
  home <- getEnv "HOME"
  return $ ensureSlash home ++ "Documents/DYMO Label Software/Labels"

fmtInt :: Int -> String
fmtInt n = printf "%03d" n

zeroPad :: String -> String
zeroPad lineNo =
  case readMaybe lineNo of
    Nothing -> lineNo
    Just n -> fmtInt n

writeLabel :: FilePath -> LineItem -> IO ()
writeLabel destDir item = do
  let lineNo = zeroPad (liLineNo item)
  let destFile = destDir ++ "Mc" ++ liPoNo item ++ "-" ++ lineNo ++ ".label"
  putStrLn $ "Writing " ++ destFile
  makeLabel destFile item

processFile :: FilePath -> FilePath -> IO ()
processFile srcFile destDir = do
  putStrLn $ "Reading " ++ srcFile
  let srcDir = dropWhileEnd (/= '/') srcFile
  items <- lineItemsFromFile srcFile
  items' <- transformItems srcDir items
  mapM_ (writeLabel destDir) items'

ensureSlash :: FilePath -> FilePath
ensureSlash "" = ""
ensureSlash dir
  | '/' == last dir = dir
  | otherwise = dir ++ "/"

main :: IO ()
main = do
  labelDir <- getLabelDir
  processFile htmlfile (ensureSlash labelDir)
