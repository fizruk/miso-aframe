-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String (ms)

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

haskellLogo :: [[Char]]
haskellLogo =
  [ "aaaaaa   xxxxxxx                       "
  , "  aaaaaa   xxxxxx                      "
  , "   aaaaaa   xxxxxxx                    "
  , "    aaaaaa   xxxxxxx                   "
  , "      aaaaaa   xxxxxx   aaaaaaaaaaaaaaa"
  , "       aaaaaa   xxxxxxx  aaaaaaaaaaaaaa"
  , "        aaaaaa   xxxxxxx               "
  , "        aaaaaa   xxxxxxxx              "
  , "       aaaaaa   xxxxxxxxxxx   aaaaaaaaa"
  , "      aaaaaa   xxxxxxxxxxxxx   aaaaaaaa"
  , "    aaaaaa   xxxxxxx   xxxxxxx         "
  , "   aaaaaa   xxxxxxx     xxxxxxx        "
  , "  aaaaaa   xxxxxx        xxxxxxx       "
  , "aaaaaa   xxxxxxx           xxxxxxx     "
  ]

asciiToVoxels :: [[Char]] -> [View action]
asciiToVoxels ascii = concat $ do
  (y, line) <- zip [0..] ascii
  (x, char) <- zip [0..] line
  return $ case char of
    'x' -> return $ voxel x y "#FFB106"
    'a' -> return $ voxel x y "#FF5900"
    _ -> []
  where
    voxel i j c = box
      [ position (s * (i - w)) (h - j) (-15)
      , color c
      , prop "width" (ms (show s))
      ]
      []

    s = 0.7   -- width:height aspect ration for ascii chars
    w = width  / 2
    h = height / 2
    width  = fromIntegral $ maximum (map length ascii)
    height = fromIntegral $ length ascii

viewModel :: Model -> View Action
viewModel _ = scene [] $
  asciiToVoxels haskellLogo ++
  [ sky
      [ color "#222222" ]
      []
  ]
