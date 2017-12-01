{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame (

  module Miso.AFrame.Primitives,
  module Miso.AFrame.Primitives.Attributes,
  module Miso.AFrame.Primitives.Camera,
  module Miso.AFrame.Primitives.Cursor,
  module Miso.AFrame.Primitives.Light,

  entity,
  scene,

  reloadAFrameApp,

) where

import Miso
import Miso.String

import Miso.AFrame.Primitives
import Miso.AFrame.Primitives.Attributes
import Miso.AFrame.Primitives.Camera
import Miso.AFrame.Primitives.Cursor
import Miso.AFrame.Primitives.Light

-- | An entity.
entity :: [Attribute action] -> [View action] -> View action
entity = node HTML "a-entity" Nothing

-- | A-Frame scene.
scene :: [Attribute action] -> [View action] -> View action
scene = node HTML "a-scene" Nothing

foreign import javascript unsafe
  "(function(){ document.body.innerHTML = ''; })();"
  clearBody :: IO ()

-- | (Re)load GHCJS app from GHCJSi.
--
-- This will clear <body> and run an app.
reloadAFrameApp :: IO () -> IO ()
reloadAFrameApp app = do
  clearBody
  app

