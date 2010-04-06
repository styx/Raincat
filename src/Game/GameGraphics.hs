module Game.GameGraphics
    (gameDraw) where

import Data.Maybe
import Graphics.UI.GLUT as Glut
import Graphics.Rendering.OpenGL
import Data.IORef
import World.World
import Nxt.Graphics
import Nxt.Types
import Rain.Rain
import Settings.WorldSettings as WorldSettings
import Settings.UISettings as UISettings
import Input.InputState as InputState
import Items.Items
import Panels.MainPanel
import Panels.ItemPanel
import Panels.MessagePanel
import Cat.Cat
import Level.EndMarker
import Level.FireHydrant
import Level.Level
import Settings.DisplaySettings

-- gameDraw
gameDraw :: IORef WorldState -> IO ()
gameDraw worldStateRef = do
    worldState <- readIORef worldStateRef

    Nxt.Graphics.begin

    let (cameraX, cameraY) = cameraPos (mainPanel worldState)

    Nxt.Graphics.worldTransform 0.0 0.0

    -- draw background
    Nxt.Graphics.drawTexture 0.0 0.0 (backgroundTexture (mainPanel worldState)) (1.0::GLdouble)

    Nxt.Graphics.worldTransform cameraX cameraY
    -- draw foreground
    sequence_ $ map (\((x, y), tex) -> Nxt.Graphics.drawTexture (x) (y) tex (1.0::GLdouble)) (levelBackgrounds $ levelData $ curLevel worldState)

    -- draw level end marker
    let endmarker = endMarker $ mainPanel worldState
        (endmarkerX, endmarkerY) = (rectX $ endMarkerRect endmarker, rectY $ endMarkerRect endmarker)
    Nxt.Graphics.drawTexture endmarkerX endmarkerY (endMarkerTexture endmarker) (1.0::GLdouble)

    -- draw fire hydrants
    let firehydrants = fireHydrants $ mainPanel worldState
    sequence_ $ map (\fh -> drawFireHydrant fh) firehydrants

    -- draw cat
    let cat' = cat (mainPanel worldState)
    drawCat cat'
    --Nxt.Graphics.drawRect (catHitbox cat') (Color4 0.5 0.5 0.5 0.5)
    --Nxt.Graphics.drawPoly (catPoly cat') (Color4 0.5 0.5 0.5 0.5)

    -- draw rain
    drawRain (raindrops $ mainPanel worldState)

    -- draw puddles
    --sequence_ $ map (\rect -> Nxt.Graphics.drawRect rect (Color4 0.0 0.0 1.0 0.5)) (puddles $ mainPanel worldState)

    -- draw rect/polygon surfaces
    --sequence_ $ map (\rect -> Nxt.Graphics.drawRect rect (Color4 1.0 0.0 0.0 0.5)) (rectSurfaces $ mainPanel worldState)
    --sequence_ $ map (\poly -> Nxt.Graphics.drawPoly poly (Color4 1.0 0.0 0.0 0.5)) (polySurfaces $ mainPanel worldState)

    -- draw items
    drawItems worldState

    Nxt.Graphics.worldTransform 0.0 0.0

    -- draw fail/win messages
    drawWinFail cat'

    -- draw item panel/message panel
    drawPanels worldState

    -- draw debug output
    --drawDebug worldState

    Nxt.Graphics.end

-- drawWinFail
drawWinFail :: Cat -> IO ()
drawWinFail cat = do
    sequence_ $ if catItemName cat == "Hurt"
                   then [Nxt.Graphics.drawRect (Rect 380.0 390.0 100.0 30.0) (Color4 0.95 0.95 0.95 1.0),
                         Nxt.Graphics.drawString 395.0 400.0 "Nooooooo :'(" (Color4 0.20 0.20 0.60 1.0)]
                   else [return ()]
    sequence_ $ if catItemName cat == "Win"
                   then [Nxt.Graphics.drawRect (Rect 380.0 390.0 120.0 30.0) (Color4 0.95 0.95 0.95 1.0),
                         Nxt.Graphics.drawString 395.0 400.0 "Stage Complete!" (Color4 0.20 0.20 0.60 1.0)]
                   else [return ()]

-- drawItems
drawItems :: WorldState -> IO ()
drawItems worldState = do
    let itemlist = (itemList (mainPanel worldState))
        corklist = (corkList (mainPanel worldState))
        tarplist = (tarpList (mainPanel worldState))

    sequence_ $ map (\item -> drawItem item) itemlist
    sequence_ $ map (\cork -> drawItem cork) corklist
    sequence_ $ map (\tarp -> drawItem tarp) tarplist

    let (cameraX, cameraY) = cameraPos (mainPanel worldState)
    mousePos <- readIORef (mousePosRef worldState)
    Glut.Size winW winH <- Glut.get Glut.windowSize
    let (mousex, mousey) = translateMousePos mousePos winW winH

    let placingItem' = placingItem $ mainPanel worldState
    if isJust placingItem'
        then drawItemAt (mousex - cameraX) (mousey - cameraY) (fromJust placingItem')
        else return ()

-- drawPanels
drawPanels :: WorldState -> IO ()
drawPanels worldState = do
    -- panel rectangles
    Nxt.Graphics.drawRect UISettings.toolsPanelRect UISettings.toolsPanelColor

    -- item panel: item buttons, item constraints
    let itemList = itemButtonList (itemPanel worldState)
    sequence_ $ map drawItemBut (init itemList)
    sequence_ $ map (\(ItemButton (x, y) _ _ _ count) -> Nxt.Graphics.drawString ((fromIntegral $ round (60.0 + x))::GLfloat)
                                                                                 ((fromIntegral $ round y)::GLfloat)
                                                                                 (show count) (Color4 0.0 0.0 0.0 1.0)) (init itemList)

    -- item panel: go/stop button
    drawGoStopButton (goStopButton $ itemPanel worldState)

    -- message panel: message
    let messagePanelStr = messageDisplay (messagePanel worldState)
    if messagePanelStr /= ""
       then sequence_ [Nxt.Graphics.drawRect UISettings.messagePanelRect UISettings.messagePanelColor,
                       Nxt.Graphics.drawString 80.0 739.0 messagePanelStr (Color4 0.0 0.0 0.0 1.0)]
       else return()

-- drawDebug
drawDebug :: WorldState -> IO ()
drawDebug worldState = do
    -- raindrop count
    let rain = raindrops (mainPanel worldState)
    Nxt.Graphics.drawString 10.0 720.0 ("Active raindrops: " ++ (show $ length rain)) (Color4 1.0 1.0 1.0 1.0)

    -- mouse cursor position
    mousePos <- readIORef (mousePosRef worldState)
    let mousex = mouseX mousePos
    let mousey = mouseY mousePos
    Nxt.Graphics.drawString 10.0 740.0 ("Mouse Pos: (" ++ (show mousex) ++ ", " ++ (show mousey) ++ ")") (Color4 0.7 0.7 0.7 1.0)
