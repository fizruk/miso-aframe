{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives.Box where

import Miso
import Miso.String

box :: Float -> Attribute action
box = prop "metalness" . ms . show

