module Menu.Menu
    (menuMain,
     menuDraw,
     howtoMain,
     howtoDraw) where

import Graphics.Rendering.OpenGL as GL hiding (get)
import qualified Graphics.UI.GLUT as Glut
import Data.IORef
import Data.Time.Clock
import World.World
import Nxt.Graphics
import Nxt.Types
import Settings.DisplaySettings
import Game.GameMain
import Game.GameGraphics
import Game.GameState
import Input.InputState
import Control.Monad.State
import Level.Level
import Settings.Path

howtoRect = Nxt.Types.Rect 690.0 470.0 200.0 60.0
backRect  = Nxt.Types.Rect 785.0 626.0 200.0 60.0

lvl1Rect = Nxt.Types.Rect 543.0 245.0 90.0 90.0
lvl2Rect = Nxt.Types.Rect 649.0 245.0 90.0 90.0
lvl3Rect = Nxt.Types.Rect 753.0 245.0 90.0 90.0
lvl4Rect = Nxt.Types.Rect 543.0 140.0 90.0 90.0
lvl5Rect = Nxt.Types.Rect 649.0 140.0 90.0 90.0
lvl6Rect = Nxt.Types.Rect 753.0 140.0 90.0 90.0
lvl7Rect = Nxt.Types.Rect 543.0  37.0 90.0 90.0
lvl8Rect = Nxt.Types.Rect 649.0  37.0 90.0 90.0
lvl9Rect = Nxt.Types.Rect 753.0  37.0 90.0 90.0

-- menuMain
menuMain :: IORef WorldState -> (IORef WorldState -> IO ()) -> IO ()
menuMain worldStateRef mainCallback = do
    dataPath   <- getDataDir
    startTime  <- getCurrentTime

    worldState <- readIORef worldStateRef
    keys'      <- readIORef (keysStateRef worldState)

    Size winW winH <- Glut.get Glut.windowSize
    mousePos <- readIORef (mousePosRef worldState)
    let (mousex, mousey) = translateMousePos mousePos winW winH

    let gameState' = if pointInRect (mousex, mousey) howtoRect && lMouseDown keys'
                        then HowtoMenuState
                        else MainMenuState

    worldState' <- execStateT (do
                                    -- level 1
                                    w <- get
                                    lvl1 <- if pointInRect (mousex, mousey) lvl1Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/water1/water1.lvl")
                                                else return w
                                    lvl1Bg <- if pointInRect (mousex, mousey) lvl1Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/water1/water1.lvl") (curLevel lvl1)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl1
                                    put (lvl1 {curLevel = (curLevel lvl1) {levelData = (levelData (curLevel lvl1)) {levelBackgrounds = lvl1Bg}}})

                                    -- level 2
                                    w <- get
                                    lvl2 <- if pointInRect (mousex, mousey) lvl2Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/movement1/movement1.lvl")
                                                else return w
                                    lvl2Bg <- if pointInRect (mousex, mousey) lvl2Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/movement1/movement1.lvl") (curLevel lvl2)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl2
                                    put (lvl2 {curLevel = (curLevel lvl2) {levelData = (levelData (curLevel lvl2)) {levelBackgrounds = lvl2Bg}}})

                                    -- level 3
                                    w <- get
                                    lvl3 <- if pointInRect (mousex, mousey) lvl3Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/water2/water2.lvl")
                                                else return w
                                    lvl3Bg <- if pointInRect (mousex, mousey) lvl3Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/water2/water2.lvl") (curLevel lvl3)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl3
                                    put (lvl3 {curLevel = (curLevel lvl3) {levelData = (levelData (curLevel lvl3)) {levelBackgrounds = lvl3Bg}}})

                                    -- level 4
                                    w <- get
                                    lvl4 <- if pointInRect (mousex, mousey) lvl4Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/movement2/movement2.lvl")
                                                else return w
                                    lvl4Bg <- if pointInRect (mousex, mousey) lvl4Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/movement2/movement2.lvl") (curLevel lvl4)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl4
                                    put (lvl4 {curLevel = (curLevel lvl4) {levelData = (levelData (curLevel lvl4)) {levelBackgrounds = lvl4Bg}}})

                                    -- level 5
                                    w <- get
                                    lvl5 <- if pointInRect (mousex, mousey) lvl5Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/pool/pool.lvl")
                                                else return w
                                    lvl5Bg <- if pointInRect (mousex, mousey) lvl5Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/pool/pool.lvl") (curLevel lvl5)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl5
                                    put (lvl5 {curLevel = (curLevel lvl5) {levelData = (levelData (curLevel lvl5)) {levelBackgrounds = lvl5Bg}}})

                                    -- level 6
                                    w <- get
                                    lvl6 <- if pointInRect (mousex, mousey) lvl6Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/rift/rift.lvl")
                                                else return w
                                    lvl6Bg <- if pointInRect (mousex, mousey) lvl6Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/rift/rift.lvl") (curLevel lvl6)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl6
                                    put (lvl6 {curLevel = (curLevel lvl6) {levelData = (levelData (curLevel lvl6)) {levelBackgrounds = lvl6Bg}}})

                                    -- level 7
                                    w <- get
                                    lvl7 <- if pointInRect (mousex, mousey) lvl7Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/skyline/skyline.lvl")
                                                else return w
                                    lvl7Bg <- if pointInRect (mousex, mousey) lvl7Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/skyline/skyline.lvl") (curLevel lvl7)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl7
                                    put (lvl7 {curLevel = (curLevel lvl7) {levelData = (levelData (curLevel lvl7)) {levelBackgrounds = lvl7Bg}}})

                                    -- level 8
                                    w <- get
                                    lvl8 <- if pointInRect (mousex, mousey) lvl8Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/river/river.lvl")
                                                else return w
                                    lvl8Bg <- if pointInRect (mousex, mousey) lvl8Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/river/river.lvl") (curLevel lvl8)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl8
                                    put (lvl8 {curLevel = (curLevel lvl8) {levelData = (levelData (curLevel lvl8)) {levelBackgrounds = lvl8Bg}}})

                                    -- level 9
                                    w <- get
                                    lvl9 <- if pointInRect (mousex, mousey) lvl9Rect && lMouseDown keys'
                                                then liftIO $ loadLevel w (dataPath ++ "/data/levels/pinball/pinball.lvl")
                                                else return w
                                    lvl9Bg <- if pointInRect (mousex, mousey) lvl9Rect && lMouseDown keys'
                                                 then liftIO $ loadLevelBackgrounds  (dataPath ++ "/data/levels/pinball/pinball.lvl") (curLevel lvl9)
                                                 else return $ levelBackgrounds $ levelData $ curLevel lvl9
                                    put (lvl9 {curLevel = (curLevel lvl9) {levelData = (levelData (curLevel lvl9)) {levelBackgrounds = lvl9Bg}}})

                                    return ())
                                (worldState {gameState = gameState'})

    writeIORef worldStateRef worldState'

    Glut.postRedisplay Nothing
    endTime <- getCurrentTime

    let timeDiff = truncate (1000 * diffUTCTime endTime startTime)
        timeSleep = if timeDiff < refreshMS then refreshMS - timeDiff else 0

    Glut.addTimerCallback timeSleep (mainCallback worldStateRef)

-- menuDraw
menuDraw :: IORef WorldState -> IO ()
menuDraw worldStateRef = do
    worldState <- readIORef worldStateRef

    Nxt.Graphics.begin

    Nxt.Graphics.drawTexture 0.0 0.0 (fst $ menuTextures worldState) (1.0::GLdouble)

    {-Nxt.Graphics.drawRect howtoRect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl1Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl2Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl3Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl4Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl5Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl6Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl7Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl8Rect (Color4 0.0 1.0 1.0 0.5)
    Nxt.Graphics.drawRect lvl9Rect (Color4 0.0 1.0 1.0 0.5)-}

    -- mouse cursor position
    {-mousePos <- readIORef (mousePosRef worldState)
    let mousex = mouseX mousePos
    let mousey = mouseY mousePos
    Nxt.Graphics.drawString 10.0 740.0 ("Mouse Pos: (" ++ (show mousex) ++ ", " ++ (show mousey) ++ ")") (Color4 0.7 0.7 0.7 1.0)-}

    Nxt.Graphics.end

-- howtoMain
howtoMain :: IORef WorldState -> (IORef WorldState -> IO ()) -> IO ()
howtoMain worldStateRef mainCallback = do
    startTime <- getCurrentTime

    worldState <- readIORef worldStateRef
    keys' <- readIORef (keysStateRef worldState)
    Size winW winH <- Glut.get Glut.windowSize
    mousePos <- readIORef (mousePosRef worldState)
    let (mousex, mousey) = translateMousePos mousePos winW winH

    let gameState' = if pointInRect (mousex, mousey) backRect && lMouseDown keys'
                        then MainMenuState
                        else HowtoMenuState

    writeIORef worldStateRef (worldState {gameState = gameState'})

    Glut.postRedisplay Nothing
    endTime <- getCurrentTime

    let timeDiff = truncate (1000 * diffUTCTime endTime startTime)
        timeSleep = if timeDiff < refreshMS then refreshMS - timeDiff else 0

    Glut.addTimerCallback timeSleep (mainCallback worldStateRef)

-- howtoDraw
howtoDraw :: IORef WorldState -> IO ()
howtoDraw worldStateRef = do
    worldState <- readIORef worldStateRef

    Nxt.Graphics.begin

    Nxt.Graphics.drawTexture 0.0 0.0 (snd $ menuTextures worldState) (1.0::GLdouble)
    --Nxt.Graphics.drawRect backRect (Color4 0.0 1.0 1.0 0.5)

    -- mouse cursor position
    {-mousePos <- readIORef (mousePosRef worldState)
    let mousex = mouseX mousePos
    let mousey = mouseY mousePos
    Nxt.Graphics.drawString 10.0 740.0 ("Mouse Pos: (" ++ (show mousex) ++ ", " ++ (show mousey) ++ ")") (Color4 0.7 0.7 0.7 1.0)-}

    Nxt.Graphics.end

