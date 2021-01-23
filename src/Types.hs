module Types
  ( LineItem (..)
  , McOptions (..)
  , LabelDirFailure (..)
  , LabelDir
  ) where

import System.IO.Error (IOError)

data LineItem = LineItem
  { liUrl :: String
  , liImg :: String
  , liTitle :: String
  , liDesc :: String
  , liCatNo :: String
  , liLineNo :: String
  , liPoNo :: String
  , liGotBreak :: !Bool
  } deriving (Eq, Ord, Show)

data McOptions = McOptions
  { mcDest     :: LabelDir
  , mcPrefix   :: String
  , mcVersion  :: !Bool
  , mcHelp     :: !Bool
  , mcSrcFiles :: [String]
  } deriving (Eq, Show)

data LabelDirFailure =
  LDFIOError IOError
  LDFNotFound [String]

type LabelDir = Either LabelDirFailure FilePath
