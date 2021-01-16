module TransformItem (transformItems) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as B8
import Data.Char
import Data.Ord
import Data.List
import Text.Read

import McMC

loadImage :: FilePath -> LineItem -> IO LineItem
loadImage srcDir item = do
  let fname = srcDir ++ liImg item
  image <- B.readFile fname
  let b64 = B64.encode image
  return $ item { liImg = B8.unpack b64 }

fixCatNo :: LineItem -> LineItem
fixCatNo item@(LineItem { liCatNo = "", liGotBreak = False }) =
  item { liDesc = ""
       , liCatNo = liDesc item
       }
fixCatNo item = item

trim :: String -> String
trim s = dropWhile isSpace $ dropWhileEnd isSpace s

trimItem :: LineItem -> LineItem
trimItem item =
  item { liTitle  = trim (liTitle item)
       , liDesc   = trim (liDesc item)
       , liCatNo  = trim (liCatNo item)
       , liLineNo = trim (liLineNo item)
       , liPoNo   = trim (liPoNo item)
       }

fixItem :: LineItem -> LineItem
fixItem = trimItem . fixCatNo

getLineNo :: LineItem -> Maybe Integer
getLineNo = readMaybe . liLineNo

sortItems :: [LineItem] -> [LineItem]
sortItems = sortBy (comparing getLineNo)

transformItems :: FilePath -> [LineItem] -> IO [LineItem]
transformItems srcDir items = do
  let items' = sortItems $ map fixItem items
  mapM (loadImage srcDir) items'
