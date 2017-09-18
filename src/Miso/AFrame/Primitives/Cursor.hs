{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives.Cursor where

import Miso
import Miso.String

cursor :: [Attribute action] -> [View action] -> View action
cursor = node HTML "a-cursor" Nothing

maxDistance :: Int -> Attribute action
maxDistance = prop "max-distance" . ms . show

timeout :: Int -> Attribute action
timeout = prop "timeout" . ms . show

raycaster :: MisoString -> Attribute action
raycaster = prop "raycaster"

fuse :: Bool -> Attribute action
fuse = prop "fuse" . toLower . ms . show
