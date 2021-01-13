module Main where

import Data.Char
import Data.List

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

main :: IO ()
main = do
  items <- lineItemsFromFile htmlfile
  mapM_ (putStrLn . show . fixItem) items
