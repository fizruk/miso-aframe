{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Sphere where

import Data.Aeson (ToJSON(..), Value)
import GHC.Generics (Generic)

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data SphereAttrs = SphereAttrs
  { sphereAmbientOcclusionMap            :: Maybe Value
  , sphereAmbientOcclusionMapIntensity   :: Maybe Float
  , sphereAmbientOcclusionTextureOffset  :: Maybe Vec2
  , sphereAmbientOcclusionTextureRepeat  :: Maybe Vec2
  , sphereColor                          :: Maybe Color
  , sphereDisplacementBias               :: Maybe Float
  , sphereDisplacementMap                :: Maybe Value
  , sphereDisplacementScale              :: Maybe Float
  , sphereDisplacementTextureOffset      :: Maybe Vec2
  , sphereDisplacementTextureRepeat      :: Maybe Vec2
  , sphereEnvMap                         :: Maybe Value
  , sphereFog                            :: Maybe Bool
  , sphereHeight                         :: Maybe Float
  , sphereMetalness                      :: Maybe Float
  , sphereNormalMap                      :: Maybe Value
  , sphereNormalScale                    :: Maybe Vec2
  , sphereNormalTextureOffset            :: Maybe Vec2
  , sphereNormalTextureRepeat            :: Maybe Vec2
  , spherePhiLength                      :: Maybe Float
  , spherePhiStart                       :: Maybe Float
  , sphereRadius                         :: Maybe Float
  , sphereRepeat                         :: Maybe Vec2
  , sphereRoughness                      :: Maybe Float
  , sphereSegmentsDepth                  :: Maybe Float
  , sphereSegmentsHeight                 :: Maybe Float
  , sphereSegmentsWidth                  :: Maybe Float
  , sphereSphericalEnvMap                :: Maybe Value
  , sphereSrc                            :: Maybe Value
  , sphereThetaLength                    :: Maybe Float
  , sphereThetaStart                     :: Maybe Float
  , sphereWidth                          :: Maybe Float
  , sphereWireframe                      :: Maybe Bool
  , sphereWireframeLinewidth             :: Maybe Float
  } deriving (Generic)

defaultSphereAttrs :: SphereAttrs
defaultSphereAttrs = SphereAttrs
  { sphereAmbientOcclusionMap            = Nothing
  , sphereAmbientOcclusionMapIntensity   = Nothing
  , sphereAmbientOcclusionTextureOffset  = Nothing
  , sphereAmbientOcclusionTextureRepeat  = Nothing
  , sphereColor                          = Nothing
  , sphereDisplacementBias               = Nothing
  , sphereDisplacementMap                = Nothing
  , sphereDisplacementScale              = Nothing
  , sphereDisplacementTextureOffset      = Nothing
  , sphereDisplacementTextureRepeat      = Nothing
  , sphereEnvMap                         = Nothing
  , sphereFog                            = Nothing
  , sphereHeight                         = Nothing
  , sphereMetalness                      = Nothing
  , sphereNormalMap                      = Nothing
  , sphereNormalScale                    = Nothing
  , sphereNormalTextureOffset            = Nothing
  , sphereNormalTextureRepeat            = Nothing
  , spherePhiLength                      = Nothing
  , spherePhiStart                       = Nothing
  , sphereRadius                         = Nothing
  , sphereRepeat                         = Nothing
  , sphereRoughness                      = Nothing
  , sphereSegmentsDepth                  = Nothing
  , sphereSegmentsHeight                 = Nothing
  , sphereSegmentsWidth                  = Nothing
  , sphereSphericalEnvMap                = Nothing
  , sphereSrc                            = Nothing
  , sphereThetaLength                    = Nothing
  , sphereThetaStart                     = Nothing
  , sphereWidth                          = Nothing
  , sphereWireframe                      = Nothing
  , sphereWireframeLinewidth             = Nothing
  }

instance ToJSON SphereAttrs where toJSON = gtoJSONPrimitive

sphere :: SphereAttrs -> Entity action
sphere sphereAttrs = primitive "a-sphere" attrs
  where
    attrs = attrsFromJSON sphereAttrs
