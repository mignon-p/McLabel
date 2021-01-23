module Types
  ( LineItem (..)
  , McOptions (..)
  ) where

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
  { mcDest     :: String
  , mcPrefix   :: String
  , mcVersion  :: !Bool
  , mcHelp     :: !Bool
  , mcSrcFiles :: [String]
  } deriving (Eq, Show)
