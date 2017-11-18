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

scene :: [Attribute action] -> [View action] -> View action
scene = node HTML "a-scene" Nothing

foreign import javascript unsafe
  "(function(){ document.body.innerHTML = ''; })();"
  clearBody :: IO ()

foreign import javascript unsafe
  "(function () { \
  \  var s = document.createElement('script'); \
  \  s.type = 'text/javascript'; \
  \  s.async = true; \
  \  s.src = $1; \
  \  var x = document.getElementsByTagName('script')[0]; \
  \    x.parentNode.insertBefore(s, x); \
  \  })(); "
  loadScriptAsync :: MisoString -> IO ()

-- | (Re)load an AFrame app from GHCJSi.
--
-- This will load AFrame from
-- <https://aframe.io/releases/0.6.1/aframe.min.js>.
reloadAFrameApp :: IO () -> IO ()
reloadAFrameApp app = do
  clearBody
  loadScriptAsync "https://aframe.io/releases/0.6.1/aframe.min.js"
  app

