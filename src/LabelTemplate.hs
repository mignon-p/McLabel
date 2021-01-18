{-# LANGUAGE TemplateHaskell #-}

module LabelTemplate (labelTemplate) where

import qualified Data.ByteString.UTF8 as U ( toString )
import Data.FileEmbed ( embedFile )

labelTemplate :: String
labelTemplate = U.toString $(embedFile "label-template.xml")
