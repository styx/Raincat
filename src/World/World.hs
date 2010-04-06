module World.World
    (WorldState(WorldState),
     gameState,
     keysStateRef,
     mousePosRef,
     curLevel,
     mainPanel,
     itemPanel,
     messagePanel,
     menuTextures,
     updateMessagePanel,
     updateItem,
     loadLevel,
     loadLevelBackgrounds) where

import qualified Graphics.UI.GLUT as Glut
import Data.IORef
import Input.InputState as InputState
import Panels.MainPanel
import Panels.ItemPanel
import Panels.MessagePanel
import Items.Items
import Level.Level
import Level.FireHydrant
import Level.EndMarker
import Game.GameState
import Nxt.Types
import Cat.Cat
import Nxt.Graphics
import Settings.Path

data WorldState = WorldState
    {
        gameState       :: GameState,
        keysStateRef    :: IORef InputState.KeysState,
        mousePosRef     :: IORef InputState.MousePos,
        curLevel        :: Level,
        mainPanel       :: MainPanel,
        itemPanel       :: ItemPanel,
        messagePanel    :: MessagePanel,
        menuTextures    :: (Nxt.Types.Texture, Nxt.Types.Texture)
    }

-- updateMessagePanel
updateMessagePanel :: WorldState -> IO (MessagePanel)
updateMessagePanel worldState = do
    mousePos <- readIORef (mousePosRef worldState)
    Glut.Size winW winH <- Glut.get Glut.windowSize
    let (mousex, mousey) = translateMousePos mousePos winW winH
    let mousePos' = MousePos (floor mousex) (floor mousey)
    let itemList = itemButtonList (itemPanel worldState)

    let messageDisplay' = foldr (\item str -> if mouseOverItemBut mousePos' item then (itemButDesc item) else str) "" itemList

    return (MessagePanel messageDisplay')

-- updateItem
updateItem :: WorldState -> IO (Item)
updateItem worldState = do
    mousePos <- readIORef (mousePosRef worldState)
    Glut.Size winW winH <- Glut.get Glut.windowSize
    let (mousex, mousey) = translateMousePos mousePos winW winH
    let mousePos' = MousePos (floor mousex) (floor mousey)
    let itemList = itemButtonList (itemPanel worldState)
    let cItem = curItem (mainPanel worldState)
    let newitem = foldr (\itemB r -> if mouseOverItemBut mousePos' itemB then (itemButItem itemB) else r) cItem itemList
    return newitem

-- updateItemCounts
updateItemCounts :: [ItemButton] -> [Int] -> [ItemButton]
updateItemCounts itemBtnList itemCounts =
    map (\(itemBut, count) -> itemBut {itemButCount = count}) (zip (init itemBtnList) itemCounts)
    ++
    [(last itemBtnList)]

-- loadLevel
loadLevel :: WorldState -> [Char] -> IO (WorldState)
loadLevel worldState levelPath = do
    lvl <- openLevel levelPath
    let lvlData = levelData lvl
        lvlRect = levelRects lvlData
        lvlPoly = levelPolys lvlData
        lvlPuddles = levelPuddles lvlData
        lvlEndRect = levelEnd lvlData
        lvlFireHydrantsL = levelFireHydrantsL lvlData
        lvlFireHydrantsR = levelFireHydrantsR lvlData
        lvlItemCounts = levelItemCounts lvl
        catStartRect = levelCat lvlData
        catStart = (rectX catStartRect, rectY catStartRect)
    lvlFireHydrants <- sequence $ map (\fhRect -> initFireHydrant (rectX fhRect, rectY fhRect) DirLeft) lvlFireHydrantsL
                                  ++
                                  map (\fhRect -> initFireHydrant (rectX fhRect, rectY fhRect) DirRight) lvlFireHydrantsR
    levelend <- initEndMarker (rectX lvlEndRect, rectY lvlEndRect)

    let cat' = (cat $ mainPanel worldState) {catPos = catStart}

    let initCameraPos = (0, 0)
        item = itemButItem $ last $ itemButtonList $ itemPanel worldState
        m = musak $ mainPanel worldState

    -- update item button limits/uses
    let itemButtonList' = updateItemCounts (itemButtonList $ itemPanel worldState) lvlItemCounts

    let curBGTex = backgroundTexture $ mainPanel worldState
        mainPanel' = MainPanel initCameraPos [] curBGTex cat' lvlRect lvlPoly lvlPuddles lvlFireHydrants levelend [] [] [] item Nothing m
        goStopBtn = goStopButton $ itemPanel worldState
        itemPanel' = (itemPanel $ worldState) {itemButtonList = itemButtonList', goStopButton = goStopBtn {goStopState = GoState}}

    return (worldState {curLevel = lvl, mainPanel = mainPanel', itemPanel = itemPanel',
                        gameState = GameRunningState})

-- loadLevelBackgrounds (NO TIME TO IMPLEMENT THIS IN FILE FORMAT PROPERLY!)
loadLevelBackgrounds :: [Char] -> Level -> IO ([(Vector2d, Nxt.Types.Texture)])
loadLevelBackgrounds levelPath level = do
    let lvlData = levelData level

    -- this doesn't seem to actually do anything..
    -- free previous level's textures
    --sequence_ $ map (\(_, oldBg) -> freeTexture oldBg) (levelBackgrounds lvlData)

    let lvlPos = case (drop (length dataPath) levelPath) of
                    "data/levels/water1/water1.lvl"          -> [(0.0, 0.0), (1024.0, 0.0)]
                    "data/levels/movement1/movement1.lvl"    -> [(-15.0, -265.0), (1009.0, -265.0), (2033, -265.0)]
                    "data/levels/water2/water2.lvl"          -> [(0.0, -200.0), (1024.0, -200.0)]
                    "data/levels/movement2/movement2.lvl"    -> [(5.0, -70.0), (1029.0, -70.0)]
                    "data/levels/pool/pool.lvl"              -> [(53.0, -295.0), (1077.0, -295.0)]
                    "data/levels/rift/rift.lvl"              -> [(10.0, -545.0), (1034.0, -545.0), (10.0, 479.0), (1034.0, 479.0)]
                    "data/levels/skyline/skyline.lvl"        -> [(-40.0, -685.0), (984.0, -685.0), (2008.0, -685.0),
                                                                    (-40.0, 339.0), (984.0, 339.0), (2008.0, 339.0)]
                    "data/levels/river/river.lvl"            -> [(10.0, -315.0), (1034.0, -315.0), (2058.0, -315.0)]
                    "data/levels/pinball/pinball.lvl"        -> [(110.0, -330.0), (1134.0, -330.0)]
    lvlBgs <- case (drop (length dataPath) levelPath) of
                 "data/levels/water1/water1.lvl"         -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/water1/water1_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/water1/water1_1_0.png")]
                 "data/levels/movement1/movement1.lvl"   -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/movement1/movement1_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/movement1/movement1_1_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/movement1/movement1_2_0.png")]
                 "data/levels/water2/water2.lvl"         -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/water2/water2_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/water2/water2_1_0.png")]
                 "data/levels/movement2/movement2.lvl"   -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/movement2/movement2_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/movement2/movement2_1_0.png")]
                 "data/levels/pool/pool.lvl"             -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/pool/pool_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/pool/pool_1_0.png")]
                 "data/levels/rift/rift.lvl"             -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/rift/rift_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/rift/rift_1_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/rift/rift_0_1.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/rift/rift_1_1.png")]
                 "data/levels/skyline/skyline.lvl"       -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_1_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_2_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_0_1.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_1_1.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/skyline/skyline_2_1.png")]
                 "data/levels/river/river.lvl"           -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/river/river_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/river/river_1_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/river/river_2_0.png")]
                 "data/levels/pinball/pinball.lvl"       -> sequence [Nxt.Graphics.loadTexture (dataPath ++ "data/levels/pinball/pinball_0_0.png"),
                                                                         Nxt.Graphics.loadTexture (dataPath ++ "data/levels/pinball/pinball_1_0.png")]

    return (zip lvlPos lvlBgs)

