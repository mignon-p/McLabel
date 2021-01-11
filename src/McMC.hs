module McMC (lineItemsFromFile) where

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

import Data.Maybe

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

data State = TopLevel | DtlRowNbr | TitleDtlLink | DtlRowCopy | DtlRowSpecs
           deriving (Eq, Ord, Show)

lineItem :: String -> LineItem
lineItem po = LineItem
  { liUrl = ""
  , liImg = ""
  , liTitle = ""
  , liDesc = ""
  , liCatNo = ""
  , liLineNo = ""
  , liPoNo = po
  , liGotBreak = False
  }

lineItemsFromFile :: FilePath -> IO [LineItem]
lineItemsFromFile fname = do
  trees <- getTrees fname
  return $ getLineItems (getPurchaseOrder trees) trees

getTrees :: FilePath -> IO [XmlTree]
getTrees fname = runX $ getTrees' fname

getTrees' :: FilePath -> IOSArrow b XmlTree
getTrees' fname
  = readDocument [withParseHTML yes, withWarnings no] fname
    >>>
    getXPathTrees "//input[@class='order-dtl-po']|//div[@class='dtl-row-info']"

getPurchaseOrder :: [XmlTree] -> String
getPurchaseOrder [] = ""
getPurchaseOrder (NTree tag@(XTag _ _) _ : trees)
  | isTag tag "input" = getAttr tag "value"
  | otherwise = getPurchaseOrder trees
getPurchaseOrder (_:trees) = getPurchaseOrder trees

getLineItems :: String -> [XmlTree] -> [LineItem]
getLineItems po trees = mapMaybe trees (getLineItem po)

getLineItem :: String -> XmlTree -> Maybe LineItem
getLineItem po tree@(NTree tag@(XTag _ _) _)
  | isTag tag "div" = Just $ buildLineItem (lineItem po) tree TopLevel
  | otherwise = Nothing
getLineItem _ _ = Nothing

buildLineItem :: LineItem -> XmlTree -> State -> LineItem
buildLineItem item (NTree tag@(XTag _ _) kids) state =
  let clazz = getAttr tag "class"
      href  = getAttr tag "href"
      src   = getAttr tag "src"
      (item', state') =
        if | isTag tag "div" && clazz == "dtl-row-nbr" -> (item, DtlRowNbr)
           | isTag tag "a" && clazz = "title-dtl-link" ->
               (item { liUrl = href }, TitleDtlLink)
           | isTag tag "div" && clazz == "dtl-row-copy" -> (item, DtlRowCopy)
           | isTag tag "p" && clazz == "dtl-row-specs" -> (item, DtlRowSpecs)
           | isTag tag "img" && clazz == "dtl-img" ->
               (item { liImg = src }, state)
           | isTag tag "br" && state == DtlRowSpecs ->
               (item { liGotBreak = True }, state)
           | otherwise -> (item, state)
      f li kid = buildLineItem li kid state'
  in foldl' f item' kids
buildLineItem item (NTree (XText txt) _) DtlRowCopy =
  item { liTitle = liTitle item ++ txt }
buildLineItem item (NTree (XText txt) _) DtlRowSpecs =
  if liGotBreak item
  then item { liCatNo = liCatNo item ++ txt }
  else item { liDesc = liDesc item ++ txt }
buildLineItem item (NTree (XText txt) _) DtlRowNbr =
  item { liLineNo = liLineNo item ++ txt }
buildLineItem item (NTree _ kids) state =
  let f li kid = buildLineItem li kid state
  in foldl' f item kids

isTag = undefined
getAttr = undefined
