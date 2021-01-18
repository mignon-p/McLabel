module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.IO.Unsafe
import Text.Printf
import Text.Read

import MakeLabel
import McMC
import McOptions
import TransformItem

mcOptions :: McOptions
mcOptions = McOptions
  { mcDest = unsafePerformIO getLabelDir
  , mcPrefix = "Mc"
  , mcVersion = False
  , mcHelp = False
  , mcSrcFiles = []
  }

getLabelDir :: IO FilePath
getLabelDir = do
  -- This seems to be the location on both Mac and Windows
  let subdir = "Documents/DYMO Label Software/Labels"
  home <- getHomeDirectory
  return $ ensureSlash home ++ subdir

fmtInt :: Int -> String
fmtInt n = printf "%03d" n

zeroPad :: String -> String
zeroPad lineNo =
  case readMaybe lineNo of
    Nothing -> lineNo
    Just n -> fmtInt n

writeLabel :: FilePath -> String -> LineItem -> IO ()
writeLabel destDir prefix item = do
  let lineNo = zeroPad (liLineNo item)
  let destFile = destDir ++ prefix ++ liPoNo item ++ "-" ++ lineNo ++ ".label"
  putStrLn $ "Writing " ++ destFile
  makeLabel destFile item

processFile :: FilePath -> FilePath -> String -> IO ()
processFile srcFile destDir prefix = do
  putStrLn $ "Reading " ++ srcFile
  let srcDir = dropWhileEnd (/= '/') srcFile
  items <- lineItemsFromFile srcFile
  items' <- transformItems srcDir items
  mapM_ (writeLabel destDir prefix) items'

ensureSlash :: FilePath -> FilePath
ensureSlash "" = ""
ensureSlash dir
  | '/' == last dir = dir
  | otherwise = dir ++ "/"

main :: IO ()
main = do
  opts <- parseOptionsIO mcOptions
  let labelDir = ensureSlash (mcDest opts)
  forM_ (mcSrcFiles opts) $ \htmlfile -> do
    processFile htmlfile labelDir (mcPrefix opts)
