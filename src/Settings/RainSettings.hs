module Settings.RainSettings
    (rainWidth,
     rainHeight,
     rainFallSpeed,
     rainSpawnChance,
     rainSpacing,
     rainColor) where

import Graphics.Rendering.OpenGL as GL

rainWidth :: Double
rainWidth = 1.0

rainHeight :: Double
rainHeight = 20.0

rainFallSpeed :: Double
rainFallSpeed = 20.0

-- probability is 1/rainSpawnChance, 60 times a second
rainSpawnChance :: Int
rainSpawnChance = 25

rainSpacing :: Int
rainSpacing = 1

rainColor :: [Color4 GLdouble]
rainColor = [Color4 0.0 0.0 1.0 0.3]
