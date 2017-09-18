{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives.Camera where

import Miso
import Miso.String

camera :: [Attribute action] -> [View action] -> View action
camera = node HTML "a-camera" Nothing

far :: Int -> Attribute action
far = prop "far" . ms . show

near :: Int -> Attribute action
near = prop "near" . ms . show

fov :: Int -> Attribute action
fov = prop "fov" . ms . show

lookControlsEnabled :: Bool -> Attribute action
lookControlsEnabled = prop "look-controls-enabled" . toLower . ms . show

wasdControlsEnabled :: Bool -> Attribute action
wasdControlsEnabled = prop "wasd-controls-enabled" . toLower . ms . show
