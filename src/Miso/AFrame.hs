{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame (

  module Miso.AFrame.Primitives,
  module Miso.AFrame.Primitives.Attributes,
  module Miso.AFrame.Primitives.Camera,
  module Miso.AFrame.Primitives.Cursor,
  module Miso.AFrame.Primitives.Light,

  entity,
  scene,

) where

import Miso
import Miso.String

import Miso.AFrame.Primitives
import Miso.AFrame.Primitives.Attributes
import Miso.AFrame.Primitives.Camera
import Miso.AFrame.Primitives.Cursor
import Miso.AFrame.Primitives.Light

entity :: [Attribute action] -> [View action] -> View action
entity = node HTML "a-entity" Nothing

scene :: [Attribute action] -> [View action] -> View action
scene = node HTML "a-scene" Nothing
