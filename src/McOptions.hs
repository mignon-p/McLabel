{-# LANGUAGE MultiWayIf #-}

module McOptions
  ( McOptions (..)
  , parseOptionsIO
  ) where

import Control.Monad
import Data.Version
import System.Environment
import System.Exit
import System.IO

import Paths_McLabel (version)

data McOptions = McOptions
  { mcDest     :: String
  , mcPrefix   :: String
  , mcVersion  :: !Bool
  , mcHelp     :: !Bool
  , mcSrcFiles :: [String]
  } deriving (Eq, Show)

data Option = OptionArg (McOptions -> String -> McOptions)
            | OptionFlag (McOptions -> McOptions)

parseOptions :: McOptions -> [String] -> Either String McOptions
parseOptions defaults [] = return defaults
parseOptions defaults (arg:rest) = do
  let (dashes, arg') = span (== '-') arg
      (name, arg'') = span (/= '=') arg'
      nDashes = length dashes
  if nDashes < 1
    then parseOptions (addSrcFile defaults arg) rest
    else do
    opt <- findOption nDashes name
    case opt of
      OptionFlag f -> parseOptions (f defaults) rest
      OptionArg f -> do
        (value, rest') <- case (arg'', rest) of
                            ("", next:others) -> return (next, others)
                            ('=':val, _) -> return (val, rest)
                            _ -> Left $ name ++ " requires a value"
        parseOptions (f defaults value) rest'

addSrcFile :: McOptions -> String -> McOptions
addSrcFile opts name = opts { mcSrcFiles = mcSrcFiles opts ++ [name] }

setDestDir :: McOptions -> String -> McOptions
setDestDir opts dir = opts { mcDest = dir }

setPrefix :: McOptions -> String -> McOptions
setPrefix opts pfx = opts { mcPrefix = pfx }

setVersion :: McOptions -> McOptions
setVersion opts = opts { mcVersion = True }

setHelp :: McOptions -> McOptions
setHelp opts = opts { mcHelp = True }

findOption :: Int -> String -> Either String Option
findOption 1 "d"        = return $ OptionArg setDestDir
findOption 2 "dest-dir" = return $ OptionArg setDestDir
findOption 1 "p"        = return $ OptionArg setPrefix
findOption 2 "prefix"   = return $ OptionArg setPrefix
findOption 1 "v"        = return $ OptionFlag setVersion
findOption 2 "version"  = return $ OptionFlag setVersion
findOption 1 "h"        = return $ OptionFlag setHelp
findOption 2 "help"     = return $ OptionFlag setHelp
findOption n name = Left $ "Invalid option " ++ replicate n '-' ++ name

usage :: McOptions -> [String]
usage opts =
  [ "Usage: mclabel [options] HTMLFILE ..."
  , ""
  , "-d  --dest-dir=DIR            Set directory for output files"
  , mkDefault (mcDest opts)
  , "-p  --prefix=STRING           Set prefix for output file names"
  , mkDefault (mcPrefix opts)
  , ""
  , "-v  --version                 Print version and exit"
  , "-h  --help                    Print this message and exit"
  ]

mkDefault :: Show a => a -> String
mkDefault value =
  let msg = "(Default: " ++ show value ++ ")"
      maxIndent = 32
      minIndent = 79 - length msg
      indent = if | minIndent < 0 -> 0
                  | minIndent < maxIndent -> minIndent
                  | otherwise -> maxIndent
  in replicate indent ' ' ++ msg

versionString :: String
versionString = "McLabel " ++ showVersion version

parseOptionsIO :: McOptions -> IO McOptions
parseOptionsIO defaults = do
  let putErr = hPutStrLn stderr
  args <- getArgs
  let eth = parseOptions defaults args
  case eth of
    Left msg -> do
      putErr msg
      putErr ""
      mapM_ putErr $ usage defaults
      exitFailure
    Right opts -> do
      when (mcHelp opts) $ do
        mapM_ putErr $ usage defaults
        exitSuccess
      when (mcVersion opts) $ do
        putErr versionString
        exitSuccess
      return opts
