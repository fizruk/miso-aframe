{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Cursor where

import Data.Aeson (ToJSON(..), Value)
import GHC.Generics (Generic)
import Miso

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data CursorAttrs = CursorAttrs
  { cursorFuse        :: Maybe Bool
  , cursorFuseTimeout :: Maybe Milliseconds
  , cursorMaxDistance :: Maybe Float
  } deriving (Generic)

instance ToJSON CursorAttrs where toJSON = gtoJSONPrimitive

defaultCursorAttrs :: CursorAttrs
defaultCursorAttrs = CursorAttrs
  { cursorFuse        = Nothing
  , cursorFuseTimeout = Nothing
  , cursorMaxDistance = Nothing
  }

cursor :: CursorAttrs -> Entity action
cursor = primitive "a-cursor" . attrsFromJSON
