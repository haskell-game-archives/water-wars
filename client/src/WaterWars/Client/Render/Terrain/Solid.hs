module WaterWars.Client.Render.Terrain.Solid where

import Graphics.Gloss

data Solid = Solid
  { solidWidth :: Float,
    solidHeight :: Float,
    solidCenter :: (Float, Float),
    solidTexture :: Picture
  }
  deriving (Show, Eq)
