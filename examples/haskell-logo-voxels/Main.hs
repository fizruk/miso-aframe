-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

import Data.Function

-- | Miso framework import
import Miso
import Miso.String (ms, MisoString)

import Miso.AFrame.Core

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

type AsciiImage = [[Char]]

-- | Width-to-height aspect ratio for monofont used in ASCII images.
asciiRatio :: Float
asciiRatio = 0.7

-- | Haskell logo.
--
-- @o@ — orange.
-- @y@ — yellow.
haskellLogo :: AsciiImage
haskellLogo =
  [ "oooooo   yyyyyyy                       "
  , "  oooooo   yyyyyy                      "
  , "   oooooo   yyyyyyy                    "
  , "    oooooo   yyyyyyy                   "
  , "      oooooo   yyyyyy   ooooooooooooooo"
  , "       oooooo   yyyyyyy  oooooooooooooo"
  , "        oooooo   yyyyyyy               "
  , "        oooooo   yyyyyyyy              "
  , "       oooooo   yyyyyyyyyyy   ooooooooo"
  , "      oooooo   yyyyyyyyyyyyy   oooooooo"
  , "    oooooo   yyyyyyy   yyyyyyy         "
  , "   oooooo   yyyyyyy     yyyyyyy        "
  , "  oooooo   yyyyyy        yyyyyyy       "
  , "oooooo   yyyyyyy           yyyyyyy     "
  ]

voxelColor :: Char -> Maybe Color
voxelColor 'o' = Just (ColorName "#FF5900")
voxelColor 'y' = Just (ColorName "#FFB106")
voxelColor _   = Nothing

prop_ :: MisoString -> MisoString -> Attribute action
prop_ = prop

voxel :: Int -> Int -> Int -> Color -> View action
voxel i j k c = box defaultBoxAttrs
  { boxColor    = Just c
  , boxWidth    = Just asciiRatio }
  [ position (Vec3 x y z) ]
  [ animation "scale" (Just (Vec3 0 0 0)) (Vec3 1 1 1) defaultAnimationAttrs
        { animationDur       = Just 1000
        , animationFill      = Just AnimationForwards
        , animationRepeat    = Indefinite
        , animationDirection = Just AnimationAlternate
        } ]
  where
    x = fromIntegral i * asciiRatio
    y = fromIntegral j
    z = fromIntegral k

asciiToVoxels :: AsciiImage -> [View action]
asciiToVoxels ascii =
  [ voxel (i - w) (h - j) (-15) c
  | (j, line) <- zip [0..] ascii
  , (i, char) <- zip [0..] line
  , Just c <- [voxelColor char]
  ]
  where
    h = length ascii `div` 2
    w = maximum (map length ascii) `div` 2

viewModel :: Model -> View Action
viewModel _ = scene [] $
  asciiToVoxels haskellLogo ++
   [ sky defaultSkyAttrs { skyColor = Just (ColorName "#222")}
     [] []
   ]
