module Main where

import Control.Monad ( forM_, when )
import Data.List ( dropWhileEnd )
import System.Directory ( getHomeDirectory )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf ( printf )
import Text.Read ( readMaybe )

import DirSep
import MakeLabel
import McOptions
import ParseReceipt
import TransformItem
import Types

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
  putStrLn $ "  Writing " ++ destFile
  makeLabel destFile item

noItemMessage :: [String]
noItemMessage =
  [ "  <div class=\"dtl-row-info\"> not found in specified HTML file."
  , "  Possible reasons:"
  , "    1.  Input file is not the correct HTML file."
  , "        (Go to https://www.mcmaster.com/order-history and click on a"
  , "        specific purchase order on the right side of the screen, and then"
  , "        save that page as \"Web Page, complete\".)"
  , "    2.  McMaster-Carr has changed their HTML format since this program"
  , "        was written."
  ]

processFile :: FilePath -> FilePath -> String -> IO ()
processFile srcFile destDir prefix = do
  putStrLn $ "Reading " ++ srcFile
  let srcDir = dropWhileEnd (/= '/') srcFile
  items <- lineItemsFromFile srcFile
  items' <- transformItems srcDir items
  when (null items') $ mapM_ putStrLn noItemMessage
  mapM_ (writeLabel destDir prefix) items'

ensureSlash :: FilePath -> FilePath
ensureSlash "" = ""
ensureSlash dir
  | last dir `elem` dirSep = dir
  | otherwise = dir ++ "/"

main :: IO ()
main = do
  opts <- parseOptionsIO mcOptions
  let labelDir = ensureSlash (mcDest opts)
  forM_ (mcSrcFiles opts) $ \htmlfile -> do
    processFile htmlfile labelDir (mcPrefix opts)
