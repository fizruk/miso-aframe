{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Core.Primitives.Text where

import Data.Aeson (ToJSON(..), Value)
import GHC.Generics (Generic)
import Miso (prop)
import Miso.String (MisoString)

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

data TextAlignment
  = TextAlignmentLeft
  | TextAlignmentCenter
  | TextAlignmentRight
  deriving (Generic)

data TextAnchor
  = TextAnchorLeft
  | TextAnchorCenter
  | TextAnchorRight
  | TextAnchorAlign
  deriving (Generic)

data TextBaseline
  = TextBaselineTop
  | TextBaselineCenter
  | TextBaselineBottom
  deriving (Generic)

data TextSide
  = TextFront
  | TextBack
  | TextDouble
  deriving (Generic)

data TextAttrs = TextAttrs
  { textAlign         :: Maybe TextAlignment
  , textAlphaTest     :: Maybe Float
  , textAnchor        :: Maybe TextAnchor
  , textBaseline      :: Maybe TextBaseline
  , textColor         :: Maybe Color
  , textFont          :: Maybe MisoString
  , textFontImage     :: Maybe MisoString
  , textHeight        :: Maybe Float
  , textLetterSpacing :: Maybe Float
  , textLineHeight    :: Maybe Float
  , textOpacity       :: Maybe Float
  , textShader        :: Maybe Value
  , textSide          :: Maybe TextSide
  , textTabSize       :: Maybe Int
  , textTansparent    :: Maybe Bool
  , textWhiteSpace    :: Maybe MisoString
  , textWidth         :: Maybe Float
  , textWrapCount     :: Maybe Int
  , textWrapPixels    :: Maybe Int
  , textZOffset       :: Maybe Float
  } deriving (Generic)

defaultTextAttrs :: TextAttrs
defaultTextAttrs = TextAttrs
  { textAlign         = Nothing
  , textAlphaTest     = Nothing
  , textAnchor        = Nothing
  , textBaseline      = Nothing
  , textColor         = Nothing
  , textFont          = Nothing
  , textFontImage     = Nothing
  , textHeight        = Nothing
  , textLetterSpacing = Nothing
  , textLineHeight    = Nothing
  , textOpacity       = Nothing
  , textShader        = Nothing
  , textSide          = Nothing
  , textTabSize       = Nothing
  , textTansparent    = Nothing
  , textWhiteSpace    = Nothing
  , textWidth         = Nothing
  , textWrapCount     = Nothing
  , textWrapPixels    = Nothing
  , textZOffset       = Nothing
  }

text :: MisoString -> TextAttrs -> Entity action
text value textAttrs = primitive "a-text" attrs
  where
    attrs
      = prop "value" value
      : attrsFromJSON textAttrs

instance ToJSON TextAttrs where toJSON = gtoJSONPrimitive

instance ToJSON TextAlignment where toJSON = gtoJSONPrimitive
instance ToJSON TextAnchor    where toJSON = gtoJSONPrimitive
instance ToJSON TextBaseline  where toJSON = gtoJSONPrimitive
instance ToJSON TextSide      where toJSON = gtoJSONPrimitive
