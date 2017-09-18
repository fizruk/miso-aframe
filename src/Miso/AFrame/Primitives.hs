{-# LANGUAGE OverloadedStrings #-}
module Miso.AFrame.Primitives where

import Miso
import Miso.String

box :: [Attribute action] -> [View action] -> View action
box = node HTML "a-box" Nothing

circle :: [Attribute action] -> [View action] -> View action
circle = node HTML "a-circle" Nothing

colladaModel :: [Attribute action] -> [View action] -> View action
colladaModel = node HTML "a-collada-model" Nothing

cone :: [Attribute action] -> [View action] -> View action
cone = node HTML "a-cone" Nothing

curvedImage :: [Attribute action] -> [View action] -> View action
curvedImage = node HTML "a-curvedimage" Nothing

cylinder :: [Attribute action] -> [View action] -> View action
cylinder = node HTML "a-cylinder" Nothing

dodecahedron :: [Attribute action] -> [View action] -> View action
dodecahedron = node HTML "a-dodecahedron" Nothing

gltfModel :: [Attribute action] -> [View action] -> View action
gltfModel = node HTML "a-gltf-model" Nothing

icosahedron :: [Attribute action] -> [View action] -> View action
icosahedron = node HTML "a-icosahedron" Nothing

image :: [Attribute action] -> [View action] -> View action
image = node HTML "a-image" Nothing

link :: [Attribute action] -> [View action] -> View action
link = node HTML "a-link" Nothing

objModel :: [Attribute action] -> [View action] -> View action
objModel = node HTML "a-obj-model" Nothing

octahedron :: [Attribute action] -> [View action] -> View action
octahedron = node HTML "a-octahedron" Nothing

plane :: [Attribute action] -> [View action] -> View action
plane = node HTML "a-plane" Nothing

ring :: [Attribute action] -> [View action] -> View action
ring = node HTML "a-ring" Nothing

sky :: [Attribute action] -> [View action] -> View action
sky = node HTML "a-sky" Nothing

sound :: [Attribute action] -> [View action] -> View action
sound = node HTML "a-sound" Nothing

sphere :: [Attribute action] -> [View action] -> View action
sphere = node HTML "a-sphere" Nothing

tetrahedron :: [Attribute action] -> [View action] -> View action
tetrahedron = node HTML "a-tetrahedron" Nothing

text :: [Attribute action] -> [View action] -> View action
text = node HTML "a-text" Nothing

torusKnot :: [Attribute action] -> [View action] -> View action
torusKnot = node HTML "a-torus-knot" Nothing

torus :: [Attribute action] -> [View action] -> View action
torus = node HTML "a-torus" Nothing

triangle :: [Attribute action] -> [View action] -> View action
triangle = node HTML "a-triangle" Nothing

video :: [Attribute action] -> [View action] -> View action
video = node HTML "a-video" Nothing

videosphere :: [Attribute action] -> [View action] -> View action
videosphere = node HTML "a-videosphere" Nothing

assets :: [Attribute action] -> [View action] -> View action
assets = node HTML "a-assets" Nothing

assetItem :: [Attribute action] -> [View action] -> View action
assetItem = node HTML "a-asset-item" Nothing
