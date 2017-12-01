{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Miso.AFrame.Core.Assets where

import Data.String (IsString)
import GHCJS.Marshal (ToJSVal)
import Miso
import Miso.String (MisoString)

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

newtype AssetId = AssetId MisoString
  deriving (IsString, ToJSVal)

newtype AssetSource = AssetSource MisoString
  deriving (IsString, ToJSVal)

assets :: Maybe Milliseconds -> [View action] -> View action
assets mtimeout = node_ "a-assets" attrs
  where
    attrs = case mtimeout of
      Nothing -> []
      Just timeout -> [ prop "timeout" timeout ]

assetItem' :: MisoString -> [Attribute action] -> AssetId -> AssetSource -> View action
assetItem' tag extraAttrs assetId assetSource = node_ tag attrs []
  where
    attrs =
      [ prop "id"  assetId
      , prop "src" assetSource
      ] ++ extraAttrs

assetItem :: AssetId -> AssetSource -> View action
assetItem = assetItem' "a-asset-item" []

img :: AssetId -> AssetSource -> View action
img = assetItem' "img" []

audio :: AssetId -> AssetSource -> View action
audio = assetItem' "audio" []

video :: AssetId -> AssetSource -> View action
video = assetItem' "video" []
