{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Components where

import Data.Aeson
import GHC.Generics (Generic)
import GHCJS.Marshal (ToJSVal(..))

import Miso (prop)

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data CameraAttrs_ = CameraAttrs_
  { cameraActive_     :: Maybe Bool
  , cameraFar_        :: Maybe Float
  , cameraFov_        :: Maybe Float
  , cameraNear_       :: Maybe Float
  , cameraUserHeight_ :: Maybe Float
  , cameraZoom_       :: Maybe Float
  } deriving (Generic)

instance ToJSON  CameraAttrs_ where toJSON = gtoJSON
instance ToJSVal CameraAttrs_ where toJSVal = toJSVal . gtoJSON

defaultCameraAttrs_ :: CameraAttrs_
defaultCameraAttrs_ = CameraAttrs_
  { cameraActive_     = Nothing
  , cameraFar_        = Nothing
  , cameraFov_        = Nothing
  , cameraNear_       = Nothing
  , cameraUserHeight_ = Nothing
  , cameraZoom_       = Nothing
  }

camera_ :: CameraAttrs_ -> Component action
camera_ = prop "camera"

position :: Vec3 -> Component action
position = prop "position"

rotation :: Vec3 -> Component action
rotation = prop "rotation"

scale :: Vec3 -> Component action
scale = prop "scale"
