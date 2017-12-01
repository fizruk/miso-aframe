{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Miso.AFrame where

import Miso
import Miso.String

foreign import javascript unsafe
  "(function(){ document.body.innerHTML = ''; })();"
  clearBody :: IO ()

startHtmlOnlyApp :: View action -> IO ()
startHtmlOnlyApp v = startApp App {..}
  where
    initialAction = ()
    model  = ()
    update = const noEff
    view   = const (() <$ v)
    events = defaultEvents
    subs   = []

startHtmlAndJSApp :: View action -> IO () -> IO ()
startHtmlAndJSApp v js = startApp App {..}
  where
    initialAction = ()
    model  = ()
    update _ m = m <# js
    view   = const (() <$ v)
    events = defaultEvents
    subs   = []

-- | (Re)load GHCJS app from GHCJSi.
--
-- This will clear <body> and run an app.
reloadAFrameApp :: IO () -> IO ()
reloadAFrameApp app = do
  clearBody
  app

