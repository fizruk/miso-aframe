{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives.Light where

import Miso
import Miso.String

light :: [Attribute action] -> [View action] -> View action
light = node HTML "a-light" Nothing

data LightType
  = Ambient
  | Directional
  | Hemisphere
  | Point
  | Spot
  deriving (Eq, Show)

lightTypeToMisoString :: LightType -> MisoString
lightTypeToMisoString Ambient     = "ambient"
lightTypeToMisoString Directional = "directional"
lightTypeToMisoString Hemisphere  = "hemisphere"
lightTypeToMisoString Point       = "point"
lightTypeToMisoString Spot        = "spot"

type_ :: LightType -> Attribute action
type_ = prop "type" . lightTypeToMisoString

intensity :: Float -> Attribute action
intensity = prop "intensity" . ms . show
