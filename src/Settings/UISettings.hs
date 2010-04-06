module Settings.UISettings
    (toolsPanelRect,
     toolsPanelColor,
     messagePanelRect,
     messagePanelColor) where

import Graphics.Rendering.OpenGL hiding (Rect)
import Nxt.Types
import Settings.DisplaySettings as DisplaySettings
import Settings.WorldSettings as WorldSettings
import Nxt.Graphics

toolsPanelRect :: Rect
toolsPanelRect = Rect maxWorldX 0.0 ((fromGLdouble screenResWidth) - maxWorldX) (fromGLdouble screenResHeight)

toolsPanelColor :: Color4 GLdouble
toolsPanelColor = Color4 0.6 0.8 0.8 1.0

messagePanelRect :: Rect
messagePanelRect = Rect 0.0 maxWorldY (rectX toolsPanelRect) ((fromGLdouble screenResHeight) - maxWorldY)

messagePanelColor :: Color4 GLdouble
messagePanelColor = Color4 0.91 0.91 0.91 1.0

