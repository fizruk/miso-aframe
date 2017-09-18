{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives.Attributes where

import Miso
import Miso.String
import qualified Data.List as List

type Color = MisoString

colorToHex :: Color -> MisoString
colorToHex = id

-- * Mesh attributes

color :: Color -> Attribute action
color = prop "color" . colorToHex

metalness :: Float -> Attribute action
metalness = prop "metalness" . ms . show

shader :: MisoString -> Attribute action
shader = prop "shader"

src :: MisoString -> Attribute action
src = prop "src"

opacity :: Float -> Attribute action
opacity = prop "opacity" . ms . show

roughness :: Float -> Attribute action
roughness = prop "roughness" . ms . show

translate :: Float -> Float -> Float -> Attribute action
translate x y z = prop "translate" $
  List.intercalate " " (List.map show [x, y, z])

transparent :: Bool -> Attribute action
transparent = prop "transparent" . toLower . ms . show

-- * Geometry attributes

radius :: Float -> Attribute action
radius = prop "radius" . ms . show

radiusTop :: Float -> Attribute action
radiusTop = prop "radiusTop" . ms . show

radiusBottom :: Float -> Attribute action
radiusBottom = prop "radiusBottom" . ms . show

position :: Float -> Float -> Float -> Attribute action
position x y z = prop "position" $
  List.intercalate " " (List.map show [x, y, z])

scale :: Float -> Float -> Float -> Attribute action
scale x y z = prop "scale" $
  List.intercalate " " (List.map show [x, y, z])

rotation :: Float -> Float -> Float -> Attribute action
rotation x y z = prop "rotation" $
  List.intercalate " " (List.map show [x, y, z])

height :: Float -> Attribute action
height = prop "height" . ms . show

width :: Float -> Attribute action
width = prop "width" . ms . show

depth :: Float -> Attribute action
depth = prop "depth" . ms . show

segmentsRadial :: Int -> Attribute action
segmentsRadial = prop "segmentsRadial" . ms . show

-- * Light attributes

angle :: Float -> Attribute action
angle = prop "angle" . ms . show

decay :: Float -> Attribute action
decay = prop "decay" . ms . show

distance :: Float -> Attribute action
distance = prop "distance" . ms . show

exponent :: Float -> Attribute action
exponent = prop "exponent" . ms . show

groundColor :: Color -> Attribute action
groundColor = prop "groundColor" . colorToHex
