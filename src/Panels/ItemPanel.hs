module Panels.ItemPanel
    (GoStopState(GoState,StopState),
     GoStopButton(GoStopButton),
     goStopState,
     goStopButtonRect,
     initGoStopButton,
     updateGoStopButton,
     setGoStopButton,
     toggleGoStopButton,
     drawGoStopButton,
     ItemPanel(ItemPanel),
     itemButtonList,
     goStopButton) where

import Graphics.Rendering.OpenGL as GL
import Nxt.Types
import Nxt.Graphics
import Items.Items
import Settings.Path

data GoStopState = GoState | StopState

data GoStopButton = GoStopButton
    {
        goStopState         :: GoStopState,
        goStopButtonRect    :: Nxt.Types.Rect,
        goStopCooldown      :: Int,
        goButtonTexture     :: Nxt.Types.Texture,
        stopButtonTexture   :: Nxt.Types.Texture
    }

-- initGoStopButton
initGoStopButton :: IO GoStopButton
initGoStopButton = do
    goTex <- Nxt.Graphics.loadTexture (dataPath ++ "data/item-buttons/cat-go-button.png")
    stopTex <- Nxt.Graphics.loadTexture (dataPath ++ "data/item-buttons/cat-stop-button.png")

    let rect = Nxt.Types.Rect 870.0 0.0 128.0 90.0

    return (GoStopButton GoState rect 0 goTex stopTex)

-- updateGoStopButton
updateGoStopButton :: GoStopButton -> GoStopButton
updateGoStopButton btn@(GoStopButton _ _ cooldown _ _) =
    if cooldown > 0
       then btn {goStopCooldown = cooldown - 1}
       else btn

-- setGoStopButton
setGoStopButton :: GoStopState -> GoStopButton -> GoStopButton
setGoStopButton state btn =
    btn {goStopState = state}

-- toggleGoStopButton - toggles only if cooldown is done
toggleGoStopButton :: GoStopButton -> GoStopButton
toggleGoStopButton btn@(GoStopButton _ _ 0 _ _) =
    case goStopState btn of
         GoState    -> btn {goStopState = StopState, goStopCooldown = 30}
         StopState  -> btn {goStopState = GoState, goStopCooldown = 30}
toggleGoStopButton btn = btn

-- drawGoStopButton
drawGoStopButton :: GoStopButton -> IO ()
drawGoStopButton (GoStopButton state rect _ goTex stopTex) =
    case state of
         GoState    -> Nxt.Graphics.drawTexture (rectX rect) (rectY rect) goTex (1.0::GLdouble)
         StopState  -> Nxt.Graphics.drawTexture (rectX rect) (rectY rect) stopTex (1.0::GLdouble)

data ItemPanel = ItemPanel
    {
        itemButtonList  :: [ItemButton],
        goStopButton    :: GoStopButton
    }

