module Miso.AFrame.Core.Entity where

import Miso (Attribute, View)
import Miso.String (MisoString)
import GHCJS.Types (JSVal)
import JavaScript.Array (JSArray, toList)

import Miso.AFrame.Core.Types
import Miso.AFrame.Core.Internal.Utils

entity :: Entity action
entity = node_ "a-entity"

foreign import javascript unsafe
  "(function(){ document.querySelector($1) ; })();"
  documentQuerySelector
    :: MisoString
    -> IO JSArray

selectEntities :: MisoString -> IO [JSVal]
selectEntities query =
  toList <$> documentQuerySelector query

addChildNode :: View action -> Entity action -> Entity action
addChildNode child entity attrs = entity attrs . (child :)
