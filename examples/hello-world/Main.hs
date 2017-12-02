-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

import Miso.AFrame
import Miso.AFrame.Core

type Model = ()
type Action = ()

-- | Entry point for a miso application
main :: IO ()
main = startHtmlOnlyApp $
  div_ []
    [ scene []
        [ box defaultBoxAttrs
            { boxColor = Just "#4CC3D9"
            }
            [ position (Vec3 (-1) 0.5 (-3))
            , rotation (Vec3 0 45 0)
            ] []
        , sphere defaultSphereAttrs
            { sphereColor  = Just "#EF2D5E"
            , sphereRadius = Just 1.25
            }
            [ position (Vec3 0 1.25 (-5))
            ] []
--        , cylinder
--            [ position 1 0.75 (-3)
--            , radius 0.5
--            , height 1.5
--            , color "#FFC65D"
--            ]
--            []
--        , plane
--            [ position 0 0 (-4)
--            , rotation (-90) 0 0
--            , width 4
--            , height 4
--            , color "#7BC8A4"
--            ]
--            []
        , sky defaultSkyAttrs
            { skyColor = Just "#ECECEC"
            } [] []
        ]

    -- add extra <div> below to remove toolbar on mobile browsers
    , div_ [ style_ [("height", "1000px")] ] []
    ]
