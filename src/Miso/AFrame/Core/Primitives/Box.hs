{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Box where

import Data.Aeson (ToJSON(..), Value)
import GHC.Generics (Generic)
import Miso

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data BoxAttrs = BoxAttrs
  { boxAmbientOcclusionMap            :: Maybe Value
  , boxAmbientOcclusionMapIntensity   :: Maybe Float
  , boxAmbientOcclusionTextureOffset  :: Maybe Vec2
  , boxAmbientOcclusionTextureRepeat  :: Maybe Vec2
  , boxColor                          :: Maybe Color
  , boxDepth                          :: Maybe Float
  , boxDisplacementBias               :: Maybe Float
  , boxDisplacementMap                :: Maybe Value
  , boxDisplacementScale              :: Maybe Float
  , boxDisplacementTextureOffset      :: Maybe Vec2
  , boxDisplacementTextureRepeat      :: Maybe Vec2
  , boxEnvMap                         :: Maybe Value
  , boxFog                            :: Maybe Bool
  , boxHeight                         :: Maybe Float
  , boxMetalness                      :: Maybe Float
  , boxNormalMap                      :: Maybe Value
  , boxNormalScale                    :: Maybe Vec2
  , boxNormalTextureOffset            :: Maybe Vec2
  , boxNormalTextureRepeat            :: Maybe Vec2
  , boxRepeat                         :: Maybe Vec2
  , boxRoughness                      :: Maybe Float
  , boxSegmentsDepth                  :: Maybe Float
  , boxSegmentsHeight                 :: Maybe Float
  , boxSegmentsWidth                  :: Maybe Float
  , boxSphericalEnvMap                :: Maybe Value
  , boxSrc                            :: Maybe Value
  , boxWidth                          :: Maybe Float
  , boxWireframe                      :: Maybe Bool
  , boxWireframeLinewidth             :: Maybe Float
  } deriving (Generic)

defaultBoxAttrs :: BoxAttrs
defaultBoxAttrs = BoxAttrs
  { boxAmbientOcclusionMap            = Nothing
  , boxAmbientOcclusionMapIntensity   = Nothing
  , boxAmbientOcclusionTextureOffset  = Nothing
  , boxAmbientOcclusionTextureRepeat  = Nothing
  , boxColor                          = Nothing
  , boxDepth                          = Nothing
  , boxDisplacementBias               = Nothing
  , boxDisplacementMap                = Nothing
  , boxDisplacementScale              = Nothing
  , boxDisplacementTextureOffset      = Nothing
  , boxDisplacementTextureRepeat      = Nothing
  , boxEnvMap                         = Nothing
  , boxFog                            = Nothing
  , boxHeight                         = Nothing
  , boxMetalness                      = Nothing
  , boxNormalMap                      = Nothing
  , boxNormalScale                    = Nothing
  , boxNormalTextureOffset            = Nothing
  , boxNormalTextureRepeat            = Nothing
  , boxRepeat                         = Nothing
  , boxRoughness                      = Nothing
  , boxSegmentsDepth                  = Nothing
  , boxSegmentsHeight                 = Nothing
  , boxSegmentsWidth                  = Nothing
  , boxSphericalEnvMap                = Nothing
  , boxSrc                            = Nothing
  , boxWidth                          = Nothing
  , boxWireframe                      = Nothing
  , boxWireframeLinewidth             = Nothing
  }

instance ToJSON BoxAttrs where toJSON = gtoJSONPrimitive

box :: BoxAttrs -> Entity action
box boxAttrs = primitive "a-box" attrs
  where
    attrs = attrsFromJSON boxAttrs
