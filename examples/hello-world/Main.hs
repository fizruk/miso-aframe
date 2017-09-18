-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

import Miso.AFrame

type Model = ()
type Action = ()

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = ()            -- initial action to be executed on application load
    model  = ()                   -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel _ = noEff

viewModel :: Model -> View Action
viewModel _ = scene []
  [ box
      [ position (-1) 0.5 (-3)
      , rotation 0 45 0
      , color "#4CC3D9"
      ]
      []
  , sphere
      [ position 0 1.25 (-5)
      , radius 1.25
      , color "#EF2D5E"
      ]
      []
  , cylinder
      [ position 1 0.75 (-3)
      , radius 0.5
      , height 1.5
      , color "#FFC65D"
      ]
      []
  , plane
      [ position 0 0 (-4)
      , rotation (-90) 0 0
      , width 4
      , height 4
      , color "#7BC8A4"
      ]
      []
  , sky
      [ color "#ECECEC" ]
      []
  ]
