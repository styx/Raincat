module Menu.PostVictory
    (postVictoryMain,
     postVictoryDraw) where

import Maybe
import Graphics.Rendering.OpenGL as GL hiding (get)
import Graphics.UI.GLUT hiding (get)
import Data.IORef
import Data.Time.Clock
import World.World
import Nxt.Graphics
import Nxt.Types
import Settings.DisplaySettings
import Game.GameMain
import Game.GameState
import Input.InputState
import Control.Monad.State
import Panels.MainPanel
import Cat.Cat

-- postVictoryMain
postVictoryMain :: IORef WorldState -> (IORef WorldState -> IO ()) -> IO ()
postVictoryMain worldStateRef mainCallback = do
    startTime <- getCurrentTime

    worldState <- readIORef worldStateRef
    keys' <- readIORef (keysStateRef worldState)
    mousePos <- readIORef (mousePosRef worldState)

    let mainpanel = mainPanel $ worldState

    let c = cat $ mainPanel worldState
        catLaser = if catPos c /= (540.0, 340.0)
                      then c {catPos = (540.0, 340.0), catTexture = laserTextures $ catAnimations c,
                              catItemDuration = Just 360}
                      else c
        cat' = updateCatItemDuration $ updateCatAnim catLaser

    let gameState' = if catPos cat' == (540.0, 340.0) && isJust (catItemDuration cat') && (fromJust $ catItemDuration cat') == 1
                        then MainMenuState
                        else PostVictoryState

    writeIORef worldStateRef (worldState {gameState = gameState', mainPanel = mainpanel {cat = cat'}})

    postRedisplay Nothing
    endTime <- getCurrentTime

    let timeDiff = truncate (1000 * (diffUTCTime endTime startTime))
        timeSleep = if timeDiff < refreshMS then refreshMS - timeDiff else 0

    addTimerCallback timeSleep (mainCallback worldStateRef)

-- postVictoryDraw
postVictoryDraw :: IORef WorldState -> IO ()
postVictoryDraw worldStateRef = do
    worldState <- readIORef worldStateRef

    Nxt.Graphics.begin

    drawCat $ cat $ mainPanel worldState

    --Nxt.Graphics.drawTexture 0.0 0.0 (snd $ menuTextures worldState) (1.0::GLdouble)
    --Nxt.Graphics.drawRect backRect (Color4 0.0 1.0 1.0 0.5)

    Nxt.Graphics.end
