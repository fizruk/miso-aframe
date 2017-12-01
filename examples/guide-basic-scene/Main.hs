{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Data.Aeson
import GHC.Generics
import GHCJS.Marshal
import Miso.String

import Miso.AFrame
import Miso.AFrame.Core

-- ==========================================================
-- Step 1. Empty scene
-- ==========================================================

step1 :: IO ()
step1 = startHtmlOnlyApp $
  scene [] []

-- ==========================================================
-- Step 2. A red cube
-- ==========================================================

step2 :: IO ()
step2 = startHtmlOnlyApp $
  scene []
  [
    box defaultBoxAttrs { boxColor = Just "red" }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ] []
  ]

-- ==========================================================
-- Step 3. Environment (foreign component)
-- ==========================================================

data Env = Env
  { preset      :: MisoString
  , numDressing :: Int
  } deriving (Generic, ToJSON)

instance ToJSVal Env where toJSVal = toJSVal . toJSON

environment :: Env -> Component action
environment = foreignComponent "environment"

step3 :: IO ()
step3 = startHtmlOnlyApp $
  scene []
  [
    box defaultBoxAttrs { boxColor = Just "red" }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ] []
  , entity
      [ environment Env
         { preset      = "forest"
         , numDressing = 500
         }
      ] []
  ]

-- ==========================================================
-- Step 4. Applying an Image Texture
-- ==========================================================

step4 :: IO ()
step4 = startHtmlOnlyApp $
  scene []
  [
    box defaultBoxAttrs
      { boxColor = Just "red"
      , boxSrc   = Just "https://i.imgur.com/mYmmbrp.jpg"
      }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ] []
  , entity
      [ environment Env
         { preset      = "forest"
         , numDressing = 500
         }
      ] []
  ]

-- ==========================================================
-- Step 5. Using the Asset Management System
-- ==========================================================

step5 :: IO ()
step5 = startHtmlOnlyApp $
  scene []
  [
    assets Nothing
    [
      img "boxTexture" "https://i.imgur.com/mYmmbrp.jpg"
    ]
  , box defaultBoxAttrs
      { boxColor = Just "red"
      , boxSrc   = Just "#boxTexture"
      }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ] []
  , entity
      [ environment Env
         { preset      = "forest"
         , numDressing = 500
         }
      ] []
  ]

-- ==========================================================
-- Step 6. Adding Animation
-- ==========================================================

step6 :: IO ()
step6 = startHtmlOnlyApp $
  scene []
  [
    assets Nothing
    [
      img "boxTexture" "https://i.imgur.com/mYmmbrp.jpg"
    ]
  , box defaultBoxAttrs
      { boxColor = Just "red"
      , boxSrc   = Just "#boxTexture"
      }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ]
      [
        animation "position" Nothing (Vec3 0 5 (-5)) defaultAnimationAttrs
          { animationDirection = Just AnimationAlternate
          , animationDur       = Just 2000
          , animationRepeat    = Indefinite
          }
      ]
  , entity
      [ environment Env
         { preset      = "forest"
         , numDressing = 500
         }
      ] []
  ]

-- ==========================================================
-- Step 7. Adding interaction
-- ==========================================================

step7 :: IO ()
step7 = startHtmlOnlyApp $
  scene []
  [
    assets Nothing
    [
      img "boxTexture" "https://i.imgur.com/mYmmbrp.jpg"
    ]
  , box defaultBoxAttrs
      { boxColor = Just "red"
      , boxSrc   = Just "#boxTexture"
      }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ]
      [
        animation "scale" Nothing (Vec3 2.3 2.3 2.3) defaultAnimationAttrs
          { animationBegin = Just "mouseenter"
          , animationDur   = Just 300
          }
      , animation "scale" Nothing (Vec3 2 2 2) defaultAnimationAttrs
          { animationBegin = Just "mouseleave"
          , animationDur   = Just 300
          }
      , animation "rotation" Nothing (Vec3 360 405 45) defaultAnimationAttrs
          { animationBegin = Just "click"
          , animationDur   = Just 2000
          }
      ]
  , entity
      [ environment Env
        { preset      = "forest"
        , numDressing = 500
        }
      ] []
  , camera defaultCameraAttrs []
      [
        cursor defaultCursorAttrs [] []
      ]
  ]

-- ==========================================================
-- Step 8. Adding Text
-- ==========================================================

step8 :: IO ()
step8 = startHtmlOnlyApp $
  scene []
  [
    assets Nothing
    [
      img "boxTexture" "https://i.imgur.com/mYmmbrp.jpg"
    ]
  , box defaultBoxAttrs
      { boxColor = Just "red"
      , boxSrc   = Just "#boxTexture"
      }
      [ position (Vec3 0 2 (-5))
      , rotation (Vec3 0 45 45)
      , scale    (Vec3 2 2 2) ]
      [
        animation "scale" Nothing (Vec3 2.3 2.3 2.3) defaultAnimationAttrs
          { animationBegin = Just "mouseenter"
          , animationDur   = Just 300
          }
      , animation "scale" Nothing (Vec3 2 2 2) defaultAnimationAttrs
          { animationBegin = Just "mouseleave"
          , animationDur   = Just 300
          }
      , animation "rotation" Nothing (Vec3 360 405 45) defaultAnimationAttrs
          { animationBegin = Just "click"
          , animationDur   = Just 2000
          }
      ]
  , text "Hello, world!" defaultTextAttrs
      { textColor = Just (ColorName "black")
      }
      [ position (Vec3 (-0.9) 0.2 (-3))
      , scale    (Vec3 1.5 1.5 1.5) ] []
  , entity
      [ environment Env
        { preset      = "forest"
        , numDressing = 500
        }
      ] []
  , camera defaultCameraAttrs []
      [
        cursor defaultCursorAttrs [] []
      ]
  ]

main :: IO ()
main = step8
