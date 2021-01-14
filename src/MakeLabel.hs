module MakeLabel (makeLabel) where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

import Control.Monad
import Data.Char
import Data.List

import McMC

templateName :: String
templateName = "label-template.xml"

makeLabel :: FilePath -> LineItem -> IO ()
makeLabel fname item = void $ runX $ makeLabel' fname item

makeLabel' :: FilePath -> LineItem -> IOSArrow b c
makeLabel' fname item
  = readDocument [] templateName
    >>>
    arr (fillFields item)
    >>>
    writeDocument [withIndent yes, withOutputEncoding utf8]

fillFields :: LineItem -> XmlTree -> XmlTree
fillFields item (NTree (XText txt) kids) =
  NTree (XText (xformTxt item txt)) kids
fillFields item (NTree node kids) =
  NTree node (map (fillFields item) kids)

xformTxt :: LineItem -> String -> String
xformTxt item "INSERT:ITEM_GRAPHIC"        = liImg item
xformTxt item "INSERT:ITEM_TITLE"          = wrap (liTitle item) ++ "\n"
xformTxt item "INSERT:ITEM_DESCRIPTION"    = wrap (liDesc item)
xformTxt item "INSERT:ITEM_URL"            = liUrl item
xformTxt item "INSERT:ITEM_CATNO"          = liCatNo item
xformTxt item "INSERT:ITEM_PURCHASE_ORDER" = combinePoLine item
xformTxt _ txt = txt

combinePoLine :: LineItem -> String
combinePoLine item = liPoNo item ++ ":" ++ liLineNo item

wrap :: String -> String
wrap = wrap' 0

wrap' :: Int -> String -> String
wrap' _ "" = ""
wrap' chars s =
  let (_, s') = span isSpace s
      (word, rest) = span (not . isSpace) s'
      first = chars == 0
      space' = if first then "" else " "
  in if first || chars + length word < 20
     then space' ++ word ++ wrap' (chars + length space' + length word) rest
     else "\n" ++ wrap' 0 s'
