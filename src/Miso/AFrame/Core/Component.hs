{-# LANGUAGE RecordWildCards #-}
module Miso.AFrame.Core.Component where

import Control.Monad (join)
import Data.Aeson (object)
import GHCJS.Types
import GHCJS.Foreign (jsNull)
import GHCJS.Foreign.Callback
import GHCJS.Marshal.Pure (PToJSVal(..))
import GHCJS.Marshal (ToJSVal(..), fromJSVal)
import JavaScript.Array
import Miso (Attribute, prop)
import Miso.String (MisoString)

import Miso.AFrame.Core.Types

type ComponentName = MisoString

type ComponentSchema = JSVal

data ComponentDefinition = ComponentDefinition
  { componentSchema       :: ComponentSchema
  , componentInit         :: IO ()
  , componentUpdate       :: JSVal -> IO ()
  , componentRemove       :: IO ()
  , componentTick         :: Float -> Float -> IO ()
  , componentPause        :: IO ()
  , componentPlay         :: IO ()
  , componentUpdateSchema :: JSVal -> IO ()
  , componentDependencies :: [ComponentName]
  , componentMultiple     :: Bool
  }

emptyComponentDefinition :: ComponentDefinition
emptyComponentDefinition = ComponentDefinition
  { componentSchema       = jsNull
  , componentInit         = pure ()
  , componentUpdate       = \_oldData -> pure ()
  , componentRemove       = pure ()
  , componentTick         = \_t _dt -> pure ()
  , componentPause        = pure ()
  , componentPlay         = pure ()
  , componentUpdateSchema = \_data -> pure ()
  , componentDependencies = []
  , componentMultiple     = True
  }

foreign import javascript unsafe
  "(function(){ AFRAME.registerComponent($1, { \
  \   schema:       $2, \
  \   init:         $3, \
  \   update:       $4, \
  \   remove:       $5, \
  \   tick:         $6, \
  \   pause:        $7, \
  \   play:         $8, \
  \   updateSchema: $9, \
  \   dependencies: $10, \
  \   multiple:     $11  \
  \ }); })();"
  aframeRegisterComponent
    :: ComponentName
    -> ComponentSchema
    -> Callback (IO ())
    -> Callback (JSVal -> IO ())
    -> Callback (IO ())
    -> Callback (JSVal -> JSVal -> IO ())
    -> Callback (IO ())
    -> Callback (IO ())
    -> Callback (JSVal -> IO ())
    -> JSArray
    -> Bool
    -> IO ()

registerComponent :: ComponentName -> ComponentDefinition -> IO ()
registerComponent name ComponentDefinition{..} = join $ aframeRegisterComponent
  <$> pure name
  <*> pure componentSchema
  <*> syncCallback  ThrowWouldBlock componentInit
  <*> syncCallback1 ThrowWouldBlock componentUpdate
  <*> syncCallback  ThrowWouldBlock componentRemove
  <*> syncCallback2 ThrowWouldBlock componentTick'
  <*> syncCallback  ThrowWouldBlock componentPause
  <*> syncCallback  ThrowWouldBlock componentPlay
  <*> syncCallback1 ThrowWouldBlock componentUpdateSchema
  <*> pure (fromList (map pToJSVal componentDependencies))
  <*> pure componentMultiple
  where
    componentTick' jsT jsDT = do
      Just  t <- fromJSVal jsT
      Just dt <- fromJSVal jsT
      componentTick t dt

foreignComponent :: ToJSVal a => MisoString -> a -> Component action
foreignComponent = prop
