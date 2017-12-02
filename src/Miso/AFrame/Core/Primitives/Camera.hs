{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Camera where

import Data.Aeson (ToJSON(..), Value)
import GHC.Generics (Generic)
import Miso

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data CameraAttrs = CameraAttrs
  { cameraFar                 :: Maybe Float
  , cameraFov                 :: Maybe Float
  , cameraLookControlsEnabled :: Maybe Bool
  , cameraNear                :: Maybe Float
  , cameraUserHeight          :: Maybe Float
  , cameraReverseMouseDrag    :: Maybe Bool
  , cameraWasdControlsEnabled :: Maybe Bool
  } deriving (Generic)

instance ToJSON CameraAttrs where toJSON = gtoJSONPrimitive

defaultCameraAttrs :: CameraAttrs
defaultCameraAttrs = CameraAttrs
  { cameraFar                 = Nothing
  , cameraFov                 = Nothing
  , cameraLookControlsEnabled = Nothing
  , cameraNear                = Nothing
  , cameraUserHeight          = Nothing
  , cameraReverseMouseDrag    = Nothing
  , cameraWasdControlsEnabled = Nothing
  }

camera :: CameraAttrs -> Entity action
camera = primitive "a-camera" . attrsFromJSON
