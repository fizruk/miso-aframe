{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Scene where

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

scene :: Entity action
scene = node_ "a-scene"
