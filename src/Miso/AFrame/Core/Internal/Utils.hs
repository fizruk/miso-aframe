{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Miso.AFrame.Core.Internal.Utils where

import Data.List (intercalate)
import Miso (node, prop, Attribute, View, NS(HTML))
import Miso.String (MisoString, toMisoString)

import Data.Char (isUpper, toUpper, toLower)
import Data.Aeson (ToJSON(..), Value(..), GToJSON, genericToJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics

import Miso.AFrame.Core.Types

node_ :: MisoString -> [Attribute action] -> [View action] -> View action
node_ tag = node HTML tag Nothing

primitive :: MisoString -> [Attribute action] -> Entity action
primitive tag primitiveAttrs = node_ tag . (primitiveAttrs ++)

attrsFromJSON :: ToJSON a => a -> [Attribute action]
attrsFromJSON x = case toJSON x of
  Object o -> map mkAttr (HashMap.toList o)
  _ -> []
  where
    mkAttr (name, value) = prop (toMisoString name) value

gtoJSON :: forall a d f. (Generic a, GToJSON (Rep a), Rep a ~ D1 d f, Datatype d)
  => a -> Value
gtoJSON = genericToJSON (jsonOptions (datatypeName (Proxy3 :: Proxy3 d f a)))

gtoJSONPrimitive :: forall a d f. (Generic a, GToJSON (Rep a), Rep a ~ D1 d f, Datatype d)
  => a -> Value
gtoJSONPrimitive = genericToJSON (jsonOptionsPrimitive (datatypeName (Proxy3 :: Proxy3 d f a)))

data Proxy3 d f a = Proxy3

jsonOptions :: String -> Options
jsonOptions tname = defaultOptions
  { fieldLabelModifier     = stripCommonPrefixWords tname
  , constructorTagModifier = stripCommonPrefixWords tname
  , omitNothingFields      = True
  }

jsonOptionsPrimitive :: String -> Options
jsonOptionsPrimitive tname = options
  { fieldLabelModifier     = toKebabCase . fieldLabelModifier
  , constructorTagModifier = toKebabCase . constructorTagModifier
  }
  where
    options@Options{..} = jsonOptions tname

toKebabCase :: String -> String
toKebabCase = intercalate "-" . map (map toLower) . camelWords

camelWords :: String -> [String]
camelWords "" = []
camelWords s
  = case us of
      (_:_:_) -> us : camelWords restLs
      _       -> (us ++ ls) : camelWords rest
  where
   (us, restLs) = span  isUpper s
   (ls, rest)   = break isUpper restLs

stripCommonPrefix :: Eq a => [a] -> [a] -> [a]
stripCommonPrefix (x:xs) (y:ys) | x == y = stripCommonPrefix xs ys
stripCommonPrefix _ ys = ys

wordsToCamel :: [String] -> String
wordsToCamel [] = ""
wordsToCamel (w:ws) = map toLower w ++ concatMap capitalise ws

capitalise :: String -> String
capitalise (c:s) = toUpper c : s
capitalise "" = ""

stripCommonPrefixWords :: String -> String -> String
stripCommonPrefixWords xs ys = wordsToCamel
  (stripCommonPrefix (camelWords xs) (camelWords (capitalise ys)))
