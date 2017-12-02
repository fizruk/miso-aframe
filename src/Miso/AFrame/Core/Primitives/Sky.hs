{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Sky where

import Data.Aeson
import GHC.Generics

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data SkyAttrs = SkyAttrs
  { skyColor     :: Maybe Color
  , skyMetalness :: Maybe Float
  } deriving (Generic)

defaultSkyAttrs :: SkyAttrs
defaultSkyAttrs = SkyAttrs
  { skyColor     = Nothing
  , skyMetalness = Nothing}

instance ToJSON SkyAttrs where toJSON = gtoJSONPrimitive

sky :: SkyAttrs -> Entity action
sky = primitive "a-sky" . attrsFromJSON
