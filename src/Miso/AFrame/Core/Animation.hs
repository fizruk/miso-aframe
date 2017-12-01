{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Miso.AFrame.Core.Animation where

import Data.Aeson

import Miso
import Miso.String
import GHCJS.Marshal (ToJSVal(..))
import GHC.Generics (Generic)

import Miso.AFrame.Core.Utils

data Vec3 = Vec3 Float Float Float

instance ToJSVal Vec3 where
  toJSVal (Vec3 x y z) = toJSVal
    (show x <> " " <> show y <> " " <> show z)

newtype Milliseconds = Milliseconds Int
  deriving (Eq, Num, ToJSON)

data AnimationRepeatCount
  = Finite Int
  | Indefinite
  deriving (Generic)

-- | Determines effect of animation when not actively in play.
data AnimationFill
  = AnimationBackwards
  | AnimationBoth
  | AnimationForwards
  | AnimationNone
  deriving (Generic)

data AnimationBasicEasing
  = Ease
  | EaseIn
  | EaseOut
  | EaseInOut
  deriving (Generic)

data AnimationEasingGroup
  = Cubic
  | Quad
  | Quart
  | Quint
  | Sine
  | Expo
  | Circ
  | Elastic
  | Back
  | Bounce
  deriving (Generic)

data AnimationEasing = AnimationEasing
  { animationEasingBasic :: AnimationBasicEasing
  , animationEasingGroup :: Maybe AnimationEasingGroup
  } deriving (Generic)

data AnimationDirection
  = AnimationAlternate
  | AnimationAlternateReverse
  | AnimationNormal
  | AnimationReverse
  deriving (Show, Generic)

-- | Animation attributes.
data AnimationAttrs = AnimationAttrs
  { -- | Event name to wait on before beginning animation.
    animationBegin     :: Maybe MisoString
  , -- | Event name to wait on before stopping animation.
    animationEnd       :: Maybe MisoString
  , -- | Delay (in milliseconds) or event name to wait on before beginning animation.
    animationDelay     :: Maybe Milliseconds
  , -- | Direction of the animation (between 'animationFrom' and 'animationTo').
    animationDirection :: Maybe AnimationDirection
  , -- | Duration in (milliseconds) of the animation.
    animationDur       :: Maybe Milliseconds
  , -- | Easing function of the animation. There are very many to choose from.
    animationEasing    :: Maybe AnimationEasing
  , -- | Determines effect of animation when not actively in play.
    animationFill      :: Maybe AnimationFill
  , -- | Repeat count or 'Indefinite'.
    animationRepeat    :: AnimationRepeatCount
  } deriving (Generic)

defaultAnimationAttrs :: AnimationAttrs
defaultAnimationAttrs = AnimationAttrs
  { animationBegin     = Nothing
  , animationEnd       = Nothing
  , animationDelay     = Nothing
  , animationDirection = Nothing
  , animationDur       = Nothing
  , animationEasing    = Nothing
  , animationFill      = Nothing
  , animationRepeat    = Finite 0
  }

class ToJSVal a => CanAnimate a

instance CanAnimate Vec3
instance CanAnimate Bool
instance CanAnimate Float
-- instance CanAnimate Color

-- | A-Frame animation entity.
animation
  :: CanAnimate a
  => MisoString       -- ^ Name of an attribute to provide animation for.
  -> Maybe a          -- ^ Starting value. Specify 'Nothing' to use current value.
  -> a                -- ^ Ending value.
  -> AnimationAttrs   -- ^ Animation attributes.
  -> View action
animation attrName mfrom to animAttrs = node_ "a-animation" attrs []
  where
    attrs
      = prop "attribute" attrName
      : prop "to" to
      : attrsFromJSON animAttrs
      ++ fromProp
    fromProp = case mfrom of
      Nothing   -> []
      Just from -> [prop "from" from]

instance ToJSON AnimationFill where toJSON = gtoJSON
instance ToJSON AnimationBasicEasing where toJSON = gtoJSON
instance ToJSON AnimationEasingGroup where toJSON = gtoJSON
instance ToJSON AnimationDirection where toJSON = gtoJSON
instance ToJSON AnimationAttrs where toJSON = gtoJSON

instance ToJSON AnimationRepeatCount where
  toJSON (Finite n) = toJSON n
  toJSON Indefinite = "indefinite"

instance ToJSON AnimationEasing where
   toJSON AnimationEasing{..} =
     case (toJSON <$> animationEasingGroup, toJSON animationEasingBasic) of
       (Just (String g), String b) -> String (g <> b)
       (Nothing, b) -> b
       _ -> Null
