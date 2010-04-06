module Game.GameMain
    (gameMain) where

import Data.List
import Data.Maybe
import Control.Monad.State
import Data.IORef
import qualified Graphics.UI.GLUT as Glut
import World.World
import Game.GameGraphics
import Rain.Rain as Rain
import Nxt.Types
import Input.InputState as InputState
import Panels.MainPanel
import Panels.ItemPanel
import Panels.MessagePanel
import Settings.WorldSettings as WorldSettings
import Settings.DisplaySettings as DisplaySettings
import Settings.CatSettings as CatSettings
import Nxt.Audio
import Data.Time.Clock
import Cat.Cat
import Items.Items
import Items.ItemEffects
import Level.Level
import Level.FireHydrant
import Level.EndMarker
import Control.Concurrent
import Game.GameState
import Nxt.Graphics

-- gameMain
gameMain :: IORef WorldState -> (IORef WorldState -> IO ()) -> IO ()
gameMain worldStateRef mainCallback = do
    startTime <- getCurrentTime

    worldState <- readIORef worldStateRef

    let mainpanel = mainPanel worldState

    let lvl = curLevel worldState
        lvlData = levelData lvl

    -- get updated input
    let keysRef' = keysStateRef worldState
        mousePosRef' = mousePosRef worldState
    mousePos' <- readIORef (mousePosRef worldState)
    keys' <- readIORef (keysStateRef worldState)
    Glut.Size winW winH <- Glut.get Glut.windowSize
    let (mousex, mousey) = translateMousePos mousePos' winW winH

    -- update camera pos
    let (cameraX, cameraY) = cameraPos $ mainPanel worldState
        cameraX' = if leftKeyDown keys' && cameraX < 0.0
                      then cameraX + WorldSettings.cameraSpeed
                      else
                        if rightKeyDown keys' && cameraX > -((fromIntegral $ levelWidth lvl)::Double) + (fromGLdouble screenResWidth)
                           then cameraX - WorldSettings.cameraSpeed
                           else cameraX
        cameraY' = if upKeyDown keys' && cameraY > 0.0
                      then cameraY - WorldSettings.cameraSpeed
                      else
                        if downKeyDown keys' && cameraY < ((fromIntegral $ levelHeight lvl)::Double) - (fromGLdouble screenResHeight)
                           then cameraY + WorldSettings.cameraSpeed
                           else cameraY

    -- update rain
    rain' <- updateRain worldState

    -- update go/stop state
    let goStopState' = if catItemName c == "Hurt" && isJust (catItemDuration c) && fromJust (catItemDuration c) == 1
                          then GoState
                          else goStopState $ goStopButton $ itemPanel worldState
                       where c = cat mainpanel

    -- update go/stop button
    let goStopBtn = updateGoStopButton (goStopButton $ itemPanel worldState)
        goStopBtn' = if pointInRect (mousex, mousey) (goStopButtonRect goStopBtn) && lMouseDown keys'
                        then toggleGoStopButton goStopBtn
                        else goStopBtn {goStopState = goStopState'}

    let (cat', itemL) = updateCatAndItems goStopState' mainpanel keys' (cameraX', cameraY') (mousex, mousey) lvlData
        catUsedItems = (itemList mainpanel) \\ itemL

    -- update items
    (item', (itemList', corkList', tarpList'), placedItem, placingItem', erasedItems) <- updateItemList goStopState' worldState keys' (mousex, mousey) (cameraX', cameraY') itemL

    -- update item constraints
    let itemButList = itemButtonList $ itemPanel worldState
        itemButList' = execState (do
                                     -- placed an item in world
                                     iBL <- get
                                     put (if placedItem
                                             then map (\itemBut -> if (itemName $ itemButItem itemBut) == itemName item'
                                                                      then itemBut {itemButCount = (itemButCount itemBut) - 1}
                                                                      else itemBut) iBL
                                             else iBL)

                                     -- erased an item from world
                                     iBL <- get
                                     put (if not (null erasedItems)
                                             then foldr (\ersItemName ibList -> map (\itemBut -> if (itemName $ itemButItem itemBut) == ersItemName
                                                                                                    then itemBut {itemButCount = (itemButCount itemBut) + 1}
                                                                                                    else itemBut) ibList)
                                                        iBL erasedItems
                                              else iBL)

                                     -- cat used an item in world
                                     iBL <- get
                                     put (if not (null catUsedItems)
                                             then foldr (\usedItem ibList -> map (\itemBut -> if (itemName $ itemButItem itemBut) == itemName usedItem
                                                                                                 then itemBut {itemButCount = (itemButCount itemBut) + 1}
                                                                                                 else itemBut) ibList)
                                                        iBL catUsedItems
                                             else iBL)

                                     return ())
                                 itemButList

    -- update fire hydrants
    let fireHydrantsL = if catItemName cat' == "Wrench"
                           then foldr (\fh fhList -> if rectIntersect (catHitbox cat') (fireHydrantRect fh)
                                                        then case (fireHydrantDir fh) of
                                                                  DirLeft   -> if (fst $ catPos cat') > ((rectX $ fireHydrantRect fh) + (rectWidth $ fireHydrantRect fh))
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                                  DirRight  -> if (fst $ catPos cat') < (rectX $ fireHydrantRect fh)
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                        else fh:fhList)
                                      [] (fireHydrants $ mainPanel worldState)
                           else fireHydrants $ mainPanel worldState
    let fireHydrants' = updateFireHydrants goStopState' cat' worldState

    -- update game state (menu, post victory)
    let gameState' = if escKeyDown keys'
                        then MainMenuState
                        else if catItemName cat' == "Win" && (isJust $ catItemDuration cat') && fromJust (catItemDuration cat') == 1
                                then PostVictoryState
                                else GameRunningState

    -- update panels
    let mainPanel' = mainpanel {cameraPos = (cameraX', cameraY'), raindrops = rain', cat = cat', curItem = item',
                                itemList = itemList', corkList = corkList', tarpList = tarpList',
                                fireHydrants = fireHydrants', placingItem = placingItem'}
        itemPanel' = (itemPanel worldState) {itemButtonList = itemButList', goStopButton = goStopBtn'}
    messagePanel' <- updateMessagePanel worldState

    -- update world
    let lvl = curLevel worldState
    writeIORef worldStateRef (worldState {gameState = gameState', keysStateRef = keysRef', mousePosRef = mousePosRef', mainPanel = mainPanel', itemPanel = itemPanel', messagePanel = messagePanel'})

    Glut.postRedisplay Nothing
    endTime <- getCurrentTime

    let timeDiff = truncate (1000 * (diffUTCTime endTime startTime))
        timeSleep = if timeDiff < refreshMS then refreshMS - timeDiff else 0
    --print timeDiff

    Glut.addTimerCallback timeSleep (mainCallback worldStateRef)

-- updateFireHydrants
updateFireHydrants :: GoStopState -> Cat -> WorldState -> [FireHydrant]
updateFireHydrants GoState cat worldState =
    let enabledFHs = map (\fh -> fh {fireHydrantDisabled = False}) (fireHydrants $ mainPanel worldState)
        in map updateFireHydrant enabledFHs
updateFireHydrants StopState cat worldState =
    let fireHydrantsL = if catItemName cat == "Wrench"
                           then foldr (\fh fhList -> if rectIntersect (catHitbox cat) (fireHydrantRect fh)
                                                        then case (fireHydrantDir fh) of
                                                                  DirLeft   -> if (fst $ catPos cat) > ((rectX $ fireHydrantRect fh) + (rectWidth $ fireHydrantRect fh))
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                                  DirRight  -> if (fst $ catPos cat) < (rectX $ fireHydrantRect fh)
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                        else fh:fhList)
                                      [] (fireHydrants $ mainPanel worldState)
                           else fireHydrants $ mainPanel worldState
        in map updateFireHydrant fireHydrantsL

-- updateItemList
updateItemList :: GoStopState -> WorldState -> KeysState -> Vector2d -> Vector2d -> [Item] -> IO (Item, ([Item], [Item], [Item]), Bool, Maybe Item, [[Char]])
-- updateItemList (StopState)
updateItemList StopState worldState _ _ _ itemL = do
    let mainpanel = mainPanel worldState

    posItem <- updateItem worldState
    -- Note: need to force evaluation of posItem to prevent lazy evaluation of it (causes memory leak!)
    let forceItemEval = posItem `seq` True

    return $ if forceItemEval
                then (posItem, (itemL, corkList mainpanel, tarpList mainpanel), False, Nothing, [])
                else (posItem, (itemL, corkList mainpanel, tarpList mainpanel), False, Nothing, [])
-- updateItemList (GoState)
updateItemList GoState worldState keys (mousex, mousey) (camerax, cameray) itemL = do
    let mainpanel = mainPanel worldState
    let itemButList = itemButtonList $ itemPanel worldState

    posItem <- updateItem worldState
    let tempItem = if lMouseDown keys then posItem else curItem mainpanel
    let item' = tempItem `seq` curItem mainpanel `seq` (if isNothing (placingItem mainpanel)
                   then tempItem
                  else curItem mainpanel)
                {itemPos = (mousex - camerax - ((fromIntegral $ textureWidth $ itemTexture tempItem)::Double) / 2.0,
                                            mousey - cameray - ((fromIntegral $ textureHeight $ itemTexture tempItem)::Double) / 2.0)}
    let curItemIntersects = foldr (\item intersects -> if itemIntersects (curItem mainpanel) item
                                                          then True else intersects)
                                  False itemL
                            ||
                               foldr (\cork intersects -> if itemIntersects (curItem mainpanel) cork
                                                              then True else intersects)
                                False (corkList mainpanel)
                            ||
                               foldr (\tarp intersects -> if itemIntersects (curItem mainpanel) tarp
                                                              then True else intersects)
                                False (tarpList mainpanel)

    -- remove any items if eraser was clicked on them
    let ((itemListE, corkListE, tarpListE)) = if rMouseDown keys
                                                 then (filter (\item -> not (itemIntersects (curItem mainpanel) item)) itemL,
                                                       filter (\cork -> not (itemIntersects (curItem mainpanel) cork)) (corkList mainpanel),
                                                       filter (\tarp -> not (itemIntersects (curItem mainpanel) tarp)) (tarpList mainpanel))
                                                 else (itemL, corkList mainpanel, tarpList mainpanel)
        erasedItems = (itemL \\ itemListE) ++ ((corkList mainpanel) \\ corkListE) ++ ((tarpList mainpanel) \\ tarpListE)
        erasedItemNames = map itemName erasedItems

    -- Note: need to force evaluation of item' to prevent lazy evaluation of it (causes memory leak!)
    let forceItemEval = item' `seq` True

    -- Make sure we have at least 1 item of this to use
    let itemCountValid = if itemName item' /= "Eraser"
                            then foldr (\itemBut countValid -> if (itemName $ itemButItem itemBut) == itemName item'
                                                                  then (itemButCount itemBut) > 0
                                                                  else countValid) True itemButList
                            else False
    let placeItem = forceItemEval && lMousePrevDown keys && not (lMouseDown keys) && not curItemIntersects && mousex < maxWorldX && (itemName item') /= "Eraser" && itemCountValid && isJust (placingItem mainpanel)
    let placingItem' = if placeItem || not (lMouseDown keys)
                          then Nothing
                          else if lMouseDown keys && (itemName item') /= "Eraser" && itemCountValid
                                  then if isJust (placingItem mainpanel)
                                          then placingItem mainpanel
                                          else Just item'
                                  else Nothing

    -- placing new item in world
    let (itemList', corkList', tarpList') = if placeItem
                                               then case (itemName item') of
                                                         "Cork"    -> (itemListE, item':corkListE, tarpListE)
                                                         "Tarp"    -> (itemListE, corkListE, item':tarpListE)
                                                         "Eraser"  -> (itemListE, corkListE, tarpListE)
                                                         _         -> (item':itemListE, corkListE, tarpListE)
                                               else (itemListE, corkListE, tarpListE)

    return (item', (itemList', corkList', tarpList'), placeItem, placingItem', erasedItemNames)

-- updateCatAndItems
updateCatAndItems :: GoStopState -> MainPanel -> KeysState -> (Double, Double) -> (Double, Double) -> LevelData -> (Cat, [Item])
updateCatAndItems GoState mainpanel _ _ _ lvlData =
    let c = cat mainpanel
        catTex = catTexture c
        idleTex = head $ idleTextures $ catAnimations c
        walkTex = walkTextures $ catAnimations c
        catTex' = [idleTex] ++ walkTex
        in (c {catPos = (rectX $ levelCat lvlData, rectY $ levelCat lvlData),
               catTexture = catTex', catDirection = DirRight,
               catVelocity = (catWalkVelX, 0.0), catItemName = "NoItem",
               catItemDuration = Nothing},
            itemList mainpanel)
updateCatAndItems StopState mainpanel keys (cameraX, cameraY) (mousex, mousey) _ =
    let
        (catVelX, catVelY) = catVelocity $ cat mainpanel
        (catX, catY) = catPos $ cat mainpanel
        catdirection = catDirection $ cat mainpanel
        catrect = catHitbox $ cat mainpanel
        catpoly = catPoly $ cat mainpanel
        catitemname = catItemName $ cat mainpanel

        -- update cat and world surface collisions
        catTouchedRects = foldr (\rect touchedRects -> if rectIntersect rect catrect
                                                          then rect:touchedRects else touchedRects)
                                [] ((rectSurfaces mainpanel) ++ corkRects)
                          where corkRects = map (\cork -> itemRect cork) (corkList mainpanel)
        catTouchingPoly = foldr (\poly touching -> (polyIntersect poly catpoly) || touching)
                                False (polySurfaces mainpanel)
        catTouchingSurface = not $ null catTouchedRects || catTouchingPoly

        -- update cat and puddle collisions
        catTouchingPuddle = foldr (\puddle touching -> if rectIntersect puddle catrect
                                                          then True else touching)
                                  False (puddles mainpanel)

        catBouncePogostick = catTouchingSurface && catitemname == "Pogostick"
        catFallUmbrella = not catTouchingSurface && catVelY < 0.0 &&
                          (catitemname == "Umbrella" || catitemname == "FallUmbrella")
        catUpsUmbrella = catitemname == "UpsUmbrellaActive" || (catTouchingPuddle && catitemname == "UpsUmbrella" && catVelY < 0.0)

        -- update cat pos, direction
        (catPos', catdirection') = foldr (\rect (pos, dir) -> catRectResponse pos (catVelX, catVelY) dir catrect rect)
                                         ((catX, catY), catdirection) catTouchedRects

        -- horizontal ground velocity
        dirNeg = case catdirection' of
                     DirLeft    -> -1
                     DirRight   -> 1
        groundVelX = case catitemname of
                         "SpeedBoots"   -> catSpeedVelX * dirNeg
                         "Skateboard"   -> catSkateVelX * dirNeg
                         "Hurt"         -> 0.0
                         "Win"         -> 0.0
                         _              -> catWalkVelX * dirNeg

        -- update cat velocity
        catVel' = execState (do

                                -- gravity
                                (velX, velY) <- get
                                put (if catitemname /= "UpsUmbrellaActive" && catitemname /= "Hurt" && catitemname /= "Win"
                                        then (velX, velY + gravity)
                                        else (velX, velY))

                                -- touching rect surface
                                (velX, velY) <- get
                                put (if not $ null catTouchedRects
                                        then (groundVelX, 0.0)
                                        else (velX, velY))

                                -- touching poly surface
                                (velX, velY) <- get
                                put (if catTouchingPoly
                                        then (groundVelX, 2.0)
                                        else (velX, velY))

                                -- pogostick bounce
                                (velX, velY) <- get
                                put (if catBouncePogostick
                                        then (velX, -catVelY)
                                        else (velX, velY))

                                return ())
                            (catVelocity $ cat mainpanel)

        -- see if cat got wet
        catTouchedPuddle = foldr (\puddle touchedPuddle -> if rectIntersect puddle catrect
                                                              then Just puddle
                                                              else touchedPuddle)
                                 Nothing (puddles mainpanel)

        catTouchingRain = foldr (\rain isWet -> if rectIntersect (rainRect rain) catrect
                                                   then True
                                                   else isWet)
                                False (raindrops mainpanel)
        catTouchedFireHydrant = foldr (\fh touchedFH -> if rectIntersect (fireHydrantRect fh) catrect
                                                           then Just fh
                                                           else touchedFH)
                                Nothing (fireHydrants mainpanel)

        catWetFromRain = if catTouchingRain
                            then case catitemname of
                                      "Shield"          -> False
                                      "Poncho"          -> False
                                      "Umbrella"        -> False
                                      "FallUmbrella"    -> False
                                      _                 -> True
                            else False

        catWetFromPuddle = if isJust catTouchedPuddle
                              then case catitemname of
                                        "Shield"            -> False
                                        "UpsUmbrella"       -> False
                                        "UpsUmbrellaActive" -> False
                                        "RainBoots"         -> (rectY catrect) + (rectHeight catrect) < (rectY (fromJust catTouchedPuddle)) + (rectHeight (fromJust catTouchedPuddle))
                                        "Skateboard"        -> (rectY catrect) + (rectHeight catrect) < (rectY (fromJust catTouchedPuddle)) + (rectHeight (fromJust catTouchedPuddle))
                                        _                   -> True
                              else False

        catWetFromFireHydrant = if isJust catTouchedFireHydrant
                                   then let fh = fromJust catTouchedFireHydrant
                                            in if fireHydrantDisabled fh || catitemname == "Shield"
                                                  then False
                                                  else case fireHydrantDir fh of
                                                            DirLeft   -> if catitemname == "Poncho"
                                                                            then case catdirection of
                                                                                     DirLeft   -> False
                                                                                     _         -> rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + (rectWidth (fireHydrantRect fh)) / 2.0
                                                                            else rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + (rectWidth (fireHydrantRect fh)) / 2.0
                                                            DirRight  -> if catitemname == "Poncho"
                                                                            then case catdirection of
                                                                                      DirRight  -> False
                                                                                      _         -> rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + (rectWidth (fireHydrantRect fh))
                                                                            else rectX catrect + rectWidth catrect > rectX (fireHydrantRect fh) + 100
                                   else False

        catIsWet = catWetFromPuddle || catWetFromRain || catWetFromFireHydrant

        -- reached end marker?
        catWin = if rectIntersect catrect (endMarkerRect $ endMarker mainpanel)
                    then True
                    else False

        -- update cat item effects
        preEffect = execState (do
                                  e <- get
                                  put (if catBouncePogostick
                                          then pogostickEffect2
                                          else e)

                                  e <- get
                                  put (if catFallUmbrella
                                          then fallUmbrellaEffect
                                          else e)

                                  e <- get
                                  put (if catUpsUmbrella
                                          then upsUmbrellaEffect2
                                          else e)

                                  e <- get
                                  put (if catIsWet
                                         then hurtEffect
                                         else e)

                                  e <- get
                                  put (if catWin
                                          then winEffect
                                          else e)

                                  return ()) noEffect

        (effect, itemL) = foldr (\item (prevEff, prevList) -> if (rectIntersect (itemRect item) catrect && not ((itemName item) == "Cork" || (itemName item) == "Tarp"))
                                                                 then (itemEffect item, prevList)
                                                                 else (prevEff, item:prevList))
                                (preEffect, []) (itemList mainpanel)

        -- update cat
        cat' = execState (do
                              -- apply position change
                              c <- get
                              put (updateCatPos c catPos')

                              -- apply velocity change
                              c <- get
                              put (updateCatVel c catVel')

                              -- apply direction change
                              c <- get
                              put (c {catDirection = catdirection'})

                              -- apply item effect
                              c <- get
                              put (effect c)

                              -- update animation
                              c <- get
                              put (updateCatAnim c)

                              -- revert back to walking from spring boots
                              c <- get
                              put (if ((catItemName c) == "SpringBoots") && catTouchingSurface && catVelY < 0
                                      then walkEffect c
                                      else c)

                              -- revert back to walking from pogostick bounce
                              c <- get
                              put (if (catItemName c == "Pogostick") && (abs catVelY) <= 1.0
                                      then walkEffect c
                                      else c)

                              -- revert back to walking from falling umbrella
                              c <- get
                              put (if (catItemName c == "FallUmbrella") && catTouchingSurface
                                      then walkEffect c
                                      else c)

                              -- revert back to walking from upsidedown umbrella
                              c <- get
                              put (if (catItemName c == "UpsUmbrellaActive") && not catTouchingPuddle
                                      then walkEffect c
                                      else c)

                              -- update item duration
                              c <- get
                              put (updateCatItemDuration c)

                              -- teleport cat to mouse pos (DEBUG)
                              c <- get
                              put (if spaceKeyDown keys
                                      then updateCatPos c (mousex - cameraX, mousey - cameraY)
                                      else c)

                              return ())
                         (cat mainpanel)

        in (cat', itemL)

-- catRectResponse
catRectResponse :: Vector2d -> Vector2d -> Direction -> Nxt.Types.Rect -> Nxt.Types.Rect -> (Vector2d, Direction)
catRectResponse (catX, catY) (catVelX, catVelY) catDir catrect@(Rect catRX catRY catRW catRH) rect@(Rect rectx recty rectwidth rectheight) =
    let displaceY = (recty + rectheight) - catY
        displaceDownY = (recty + rectheight) - (catRY + catRH)
        displaceX = if catVelX < 0.0
                       then (rectx + rectwidth) - catRX
                       else if catVelX > 0.0
                               then rectx - (catRX + catRW)
                               else 0.0
        oppDir = case catDir of
                    DirLeft  -> DirRight
                    DirRight -> DirLeft
        offsetRect = overlapRect catrect rect

        in execState (do
                         -- vertical displacement
                         ((x, y), d) <- get
                         put (if catVelY > 0.0
                                 then ((x, y - displaceDownY), d)
                                 else if (abs displaceY) < (abs displaceX)
                                         then ((x, y + displaceY), d)
                                         else ((x, y), d))

                         -- horizontal displacement
                         ((x, y), d) <- get
                         put (if (catRY + catRH < recty + rectheight && catRY >= recty) || (catRY < recty && catVelY <= 0.0)
                                 then ((x + displaceX, y), oppDir)
                                 else ((x, y), d))

                         return ()) ((catX, catY), catDir)

