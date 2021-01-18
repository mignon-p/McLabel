{-# LANGUAGE TemplateHaskell #-}

module LabelTemplate (labelTemplate) where

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Data.FileEmbed

labelTemplate :: String
labelTemplate = U.toString $(embedFile "label-template.xml")
