module Miso.AFrame.Core.Scene where

import Miso (Attribute, View)

import Miso.AFrame.Core.Internal.Utils

scene :: [Attribute action] -> [View action] -> View action
scene = node_ "a-scene"
