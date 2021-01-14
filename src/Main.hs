module Main where

import Data.Char
import Data.Ord
import Data.List
import Text.Read

import MakeLabel
import McMC

htmlfile :: FilePath
htmlfile = "/Users/ppelleti/misc/0104ppelleti.html"

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

main :: IO ()
main = do
  items <- lineItemsFromFile htmlfile
  let items' = sortItems $ map fixItem items
  mapM_ (putStrLn . show) items'
