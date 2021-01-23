module Main where

import Control.Exception ( try )
import Control.Monad ( forM_, when )
import Data.List ( dropWhileEnd, find, intercalate )
import Data.Maybe ( catMaybes )
import System.Directory ( doesDirectoryExist, getHomeDirectory )
import System.IO.Error ( IOError )
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

-- "DYMO Label Software" is used on my Mac, while "DYMO Label" is
-- used on my Windows machine.  Not sure if that is a platform
-- difference, or different versions of the DYMO Label software.
-- Anyway, check them both.
labelSubdirs :: [[String]]
labelSubdirs =
  [ [ "Documents", "DYMO Label Software", "Labels" ]
  , [ "Documents", "DYMO Label", "Labels" ]
  ]

mkLabelPath :: FilePath -> [String] -> FilePath
mkLabelPath home dirs =
  -- home should end in '/' or '\' unless it is empty
  let sep = if null home
            then '/'
            else last home
  in home ++ intercalate [sep] dirs

checkLabelPath :: FilePath -> IO (Maybe FilePath)
checkLabelPath path = do
  exists <- doesDirectoryExist path
  return $ if exists then Just path else Nothing

getLabelDir :: IO LabelDir
getLabelDir = do
  eth <- getLabelDir'
  return $ case eth of
             Left ioe -> Left (LDFIOError ioe)
             Right (paths, foundPaths) -> case catMaybes foundPaths of
                                            labelDir:_ -> Right labelDir
                                            _ -> Left (LDFNotFound paths)

getLabelDir' :: IO (Either IOError ([FilePath], [Maybe FilePath]))
getLabelDir' = try $ do
  home <- getHomeDirectory
  let paths = map (mkLabelPath (ensureSlash home)) labelSubdirs
  foundPaths <- mapM checkLabelPath paths
  return (paths, foundPaths)

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
  , "        (Go to " ++ homepage ++ " and see if a new version"
  , "        is available.)"
  ]

isSep :: Char -> Bool
isSep = (`elem` dirSep)

getSep :: FilePath -> Char
getSep path =
  case find isSep path of
    Nothing -> head dirSep
    Just c -> c

processFile :: FilePath -> FilePath -> String -> IO ()
processFile srcFile destDir prefix = do
  putStrLn $ "Reading " ++ srcFile
  let srcDir = dropWhileEnd (not . isSep) srcFile
  items <- lineItemsFromFile srcFile
  items' <- transformItems srcDir items
  when (null items') $ mapM_ putStrLn noItemMessage
  mapM_ (writeLabel destDir prefix) items'

ensureSlash :: FilePath -> FilePath
ensureSlash "" = ""
ensureSlash dir
  | isSep (last dir) = dir
  | otherwise = dir ++ [getSep dir]

main :: IO ()
main = do
  opts <- parseOptionsIO mcOptions
  let labelDir = ensureSlash (mcDest opts)
  forM_ (mcSrcFiles opts) $ \htmlfile -> do
    processFile htmlfile labelDir (mcPrefix opts)
