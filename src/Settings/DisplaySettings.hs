module Settings.DisplaySettings
    (screenRes,
     screenResWidth,
     screenResHeight,
     refreshMS) where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

screenRes :: Size
screenRes = Size (truncate screenResWidth) (truncate screenResHeight)

screenResWidth :: GLdouble
screenResWidth = 1024.0

screenResHeight :: GLdouble
screenResHeight = 768.0

refreshMS :: Int
refreshMS = 16

