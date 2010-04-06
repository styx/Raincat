module Settings.WorldSettings
    (minWorldX,
     maxWorldX,
     minWorldY,
     maxWorldY,
     cameraSpeed,
     gravity,
     fireHydrantFrameTime,
     hairdryerFrameTime) where

minWorldX :: Double
minWorldX = 0.0

maxWorldX :: Double
maxWorldX = 850.0

minWorldY :: Double
minWorldY = 0.0

maxWorldY :: Double
maxWorldY = 718.0

cameraSpeed :: Double
cameraSpeed = 10.0

gravity :: Double
gravity = -0.2

fireHydrantFrameTime :: Int
fireHydrantFrameTime = 7

hairdryerFrameTime :: Int
hairdryerFrameTime = 7
