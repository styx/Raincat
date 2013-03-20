module Game.GameMain
    (gameMain) where

import Data.List
import Data.Maybe
import Control.Monad.State
import Data.IORef
import qualified Graphics.UI.GLUT as Glut
import World.World
import Rain.Rain as Rain
import Nxt.Types
import Input.InputState as InputState
import Panels.MainPanel
-- import qualified Panels.MainPanel as MainPanel
import Panels.ItemPanel
import Settings.WorldSettings as WorldSettings
import Settings.DisplaySettings as DisplaySettings
import Settings.CatSettings as CatSettings
import Data.Time.Clock
import Cat.Cat
import Items.Items
import Items.ItemEffects
import Level.Level
import Level.FireHydrant
import Level.EndMarker
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
        cameraX' 
          | leftKeyDown keys' && cameraX < 0.0 =
            cameraX + WorldSettings.cameraSpeed
          | rightKeyDown keys' &&
             cameraX >
               (-(fromIntegral $ levelWidth lvl :: Double)) +
                 fromGLdouble screenResWidth
                  = cameraX - WorldSettings.cameraSpeed
          | otherwise = cameraX
        cameraY' 
           | upKeyDown keys' && cameraY > 0.0 =
             cameraY - WorldSettings.cameraSpeed
           | downKeyDown keys' &&
               cameraY <
                 (fromIntegral $ levelHeight lvl :: Double) -
                   fromGLdouble screenResHeight
             = cameraY + WorldSettings.cameraSpeed
           | otherwise = cameraY
    -- update rain
    rain' <- updateRain worldState

    -- update go/stop state
    let goStopState' = if catItemName c == "Hurt" && (catItemDuration c == Just 1)
                          then GoState
                          else goStopState $ goStopButton $ itemPanel worldState
                       where c = cat mainpanel

    -- update go/stop button
    let goStopBtn = updateGoStopButton (goStopButton $ itemPanel worldState)
        goStopBtn' = if pointInRect (mousex, mousey) (goStopButtonRect goStopBtn) && lMouseDown keys'
                        then toggleGoStopButton goStopBtn
                        else goStopBtn {goStopState = goStopState'}

    let (cat', itemL) = updateCatAndItems goStopState' mainpanel keys' (cameraX', cameraY') (mousex, mousey) lvlData
        catUsedItems = itemList mainpanel \\ itemL

    -- update items
    (item', (itemList', corkList', tarpList'), placedItem, placingItem', erasedItems) <- updateItemList goStopState' worldState keys' (mousex, mousey) (cameraX', cameraY') itemL

    -- update item constraints
    let itemButList = itemButtonList $ itemPanel worldState
        itemButList' = execState (do
                                     -- placed an item in world
                                     iBL1 <- get
                                     put (if placedItem
                                             then map (\itemBut -> if itemName (itemButItem itemBut) == itemName item'
                                                                      then itemBut {itemButCount = itemButCount itemBut - 1}
                                                                      else itemBut) iBL1
                                             else iBL1)

                                     -- erased an item from world
                                     iBL2 <- get
                                     put (if not (null erasedItems)
                                             then foldr (\ersItemName -> map (\itemBut -> if itemName (itemButItem itemBut) == ersItemName
                                                                                                    then itemBut {itemButCount = itemButCount itemBut + 1}
                                                                                                    else itemBut))
                                                        iBL2 erasedItems
                                              else iBL2)

                                     -- cat used an item in world
                                     iBL3 <- get
                                     put (if not (null catUsedItems)
                                             then foldr (\usedItem -> map (\itemBut -> if itemName (itemButItem itemBut) == itemName usedItem
                                                                                                 then itemBut {itemButCount = itemButCount itemBut + 1}
                                                                                                 else itemBut))
                                                        iBL3 catUsedItems
                                             else iBL3)

                                     return ())
                                 itemButList

    -- update fire hydrants
    let _ = if catItemName cat' == "Wrench"
               then foldr (\fh fhList -> if rectIntersect (catHitbox cat') (fireHydrantRect fh)
                                            then case fireHydrantDir fh of
                                                      DirLeft   -> if fst (catPos cat') > (rectX (fireHydrantRect fh) + rectWidth (fireHydrantRect fh))
                                                                      then (fh {fireHydrantDisabled = True}):fhList
                                                                      else fh:fhList
                                                      DirRight  -> if fst (catPos cat') < rectX (fireHydrantRect fh)
                                                                      then (fh {fireHydrantDisabled = True}):fhList
                                                                      else fh:fhList
                                            else fh:fhList)
                          [] (fireHydrants $ mainPanel worldState)
               else fireHydrants $ mainPanel worldState
    let fireHydrants' = updateFireHydrants goStopState' cat' worldState

    -- update game state (menu, post victory)
    let gameState' 
          | escKeyDown keys' = MainMenuState
          | catItemName cat' == "Win" && (catItemDuration cat' == Just 1) =
            PostVictoryState
          | otherwise = GameRunningState

    -- update panels
    let mainPanel' = mainpanel {cameraPos = (cameraX', cameraY'), raindrops = rain', cat = cat', curItem = item',
                                itemList = itemList', corkList = corkList', tarpList = tarpList',
                                fireHydrants = fireHydrants', placingItem = placingItem'}
        itemPanel' = (itemPanel worldState) {itemButtonList = itemButList', goStopButton = goStopBtn'}
    messagePanel' <- updateMessagePanel worldState

    -- update world
    -- let lvl = curLevel worldState
    writeIORef worldStateRef (worldState {gameState = gameState', keysStateRef = keysRef', mousePosRef = mousePosRef', mainPanel = mainPanel', itemPanel = itemPanel', messagePanel = messagePanel'})

    Glut.postRedisplay Nothing
    endTime <- getCurrentTime

    let timeDiff = truncate (1000 * diffUTCTime endTime startTime)
        timeSleep = if timeDiff < refreshMS then refreshMS - timeDiff else 0
    --print timeDiff

    Glut.addTimerCallback timeSleep (mainCallback worldStateRef)

-- updateFireHydrants
updateFireHydrants :: GoStopState -> Cat -> WorldState -> [FireHydrant]
updateFireHydrants GoState _ worldState =
    let enabledFHs = map (\fh -> fh {fireHydrantDisabled = False}) (fireHydrants $ mainPanel worldState)
        in map updateFireHydrant enabledFHs
updateFireHydrants StopState theCat worldState =
    let fireHydrantsL = if catItemName theCat == "Wrench"
                           then foldr (\fh fhList -> if rectIntersect (catHitbox theCat) (fireHydrantRect fh)
                                                        then case fireHydrantDir fh of
                                                                  DirLeft   -> if fst (catPos theCat) > (rectX (fireHydrantRect fh) + rectWidth (fireHydrantRect fh))
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                                  DirRight  -> if fst (catPos theCat) < rectX (fireHydrantRect fh)
                                                                                  then (fh {fireHydrantDisabled = True}):fhList
                                                                                  else fh:fhList
                                                        else fh:fhList)
                                      [] (fireHydrants $ mainPanel worldState)
                           else fireHydrants $ mainPanel worldState
        in map updateFireHydrant fireHydrantsL

-- updateItemList
updateItemList :: GoStopState -> WorldState -> KeysState -> Vector2d -> Vector2d -> [Item] -> IO (Item, ([Item], [Item], [Item]), Bool, Maybe Item, [String])
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
                {itemPos = (mousex - camerax - (fromIntegral $ textureWidth $ itemTexture tempItem :: Double) / 2.0,
                                            mousey - cameray - (fromIntegral $ textureHeight $ itemTexture tempItem :: Double) / 2.0)}
    let curItemIntersects = foldr ((||) . itemIntersects (curItem mainpanel))
                                False itemL
                            ||
                               foldr ((||) . itemIntersects (curItem mainpanel))
                                False (corkList mainpanel)
                            ||
                               foldr ((||) . itemIntersects (curItem mainpanel))
                                False (tarpList mainpanel)

    -- remove any items if eraser was clicked on them
    let ((itemListE, corkListE, tarpListE)) = if rMouseDown keys
                                                 then (filter (not . itemIntersects (curItem mainpanel)) itemL,
                                                       filter (not . itemIntersects (curItem mainpanel)) (corkList mainpanel),
                                                       filter (not . itemIntersects (curItem mainpanel)) (tarpList mainpanel))
                                                 else (itemL, corkList mainpanel, tarpList mainpanel)
        erasedItems = (itemL \\ itemListE) ++ (corkList mainpanel \\ corkListE) ++ (tarpList mainpanel \\ tarpListE)
        erasedItemNames = map itemName erasedItems

    -- Note: need to force evaluation of item' to prevent lazy evaluation of it (causes memory leak!)
    let forceItemEval = item' `seq` True

    -- Make sure we have at least 1 item of this to use
    let itemCountValid = itemName item' /= "Eraser" &&
                            foldr (\itemBut countValid -> if itemName (itemButItem itemBut) == itemName item'
                                                                  then itemButCount itemBut > 0
                                                                  else countValid) True itemButList

    let placeItem = forceItemEval && lMousePrevDown keys && not (lMouseDown keys) && not curItemIntersects && mousex < maxWorldX && itemName item' /= "Eraser" && itemCountValid && isJust (placingItem mainpanel)
    let placingItem' 
          | placeItem || not (lMouseDown keys) = Nothing
          | lMouseDown keys && itemName item' /= "Eraser" && itemCountValid =
            if isJust (placingItem mainpanel) then placingItem mainpanel else
              Just item'
          | otherwise = Nothing

    -- placing new item in world
    let (itemList', corkList', tarpList') = if placeItem
                                               then case itemName item' of
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
        -- catTex = catTexture c
        idleTex = head $ idleTextures $ catAnimations c
        walkTex = walkTextures $ catAnimations c
        catTex' = idleTex : walkTex
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
                                [] (rectSurfaces mainpanel ++ corkRects)
                          where corkRects = map itemRect (corkList mainpanel)
        catTouchingPoly = foldr (\poly -> (polyIntersect poly catpoly ||))
                                False (polySurfaces mainpanel)
        catTouchingSurface = not $ null catTouchedRects || catTouchingPoly

        -- update cat and puddle collisions
        catTouchingPuddle = foldr (\puddle -> (rectIntersect puddle catrect ||))
                                  False (puddles mainpanel)

        catBouncePogostick = catTouchingSurface && catitemname == "Pogostick"
        catFallUmbrella = not catTouchingSurface && catVelY < 0.0 &&
                          catitemname `elem` ["Umbrella", "FallUmbrella"]
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
                                (velXg, velYg) <- get
                                put (if catitemname `notElem` ["UpsUmbrellaActive", "Hurt", "Win"]
                                        then (velXg, velYg + gravity)
                                        else (velXg, velYg))

                                -- touching rect surface
                                (velXr, velYr) <- get
                                put (if not $ null catTouchedRects
                                        then (groundVelX, 0.0)
                                        else (velXr, velYr))

                                -- touching poly surface
                                (velXp, velYp) <- get
                                put (if catTouchingPoly
                                        then (groundVelX, 2.0)
                                        else (velXp, velYp))

                                -- pogostick bounce
                                (velXpb, velYpb) <- get
                                put (if catBouncePogostick
                                        then (velXpb, -catVelY)
                                        else (velXpb, velYpb))

                                return ())
                            (catVelocity $ cat mainpanel)

        -- see if cat got wet
        catTouchedPuddle = foldr (\puddle touchedPuddle -> if rectIntersect puddle catrect
                                                              then Just puddle
                                                              else touchedPuddle)
                                 Nothing (puddles mainpanel)

        catTouchingRain = foldr (\rain -> (rectIntersect (rainRect rain) catrect ||))
                                False (raindrops mainpanel)
        catTouchedFireHydrant = foldr (\fh touchedFH -> if rectIntersect (fireHydrantRect fh) catrect
                                                           then Just fh
                                                           else touchedFH)
                                Nothing (fireHydrants mainpanel)

        catWetFromRain = catTouchingRain &&
                            case catitemname of
                                      "Shield"          -> False
                                      "Poncho"          -> False
                                      "Umbrella"        -> False
                                      "FallUmbrella"    -> False
                                      _                 -> True

        catWetFromPuddle = isJust catTouchedPuddle &&
                                case catitemname of
                                        "Shield"            -> False
                                        "UpsUmbrella"       -> False
                                        "UpsUmbrellaActive" -> False
                                        "RainBoots"         -> rectY catrect + rectHeight catrect < rectY (fromJust catTouchedPuddle) + rectHeight (fromJust catTouchedPuddle)
                                        "Skateboard"        -> rectY catrect + rectHeight catrect < rectY (fromJust catTouchedPuddle) + rectHeight (fromJust catTouchedPuddle)
                                        _                   -> True

        catWetFromFireHydrant = isJust catTouchedFireHydrant &&
                                    let fh = fromJust catTouchedFireHydrant
                                            in (not (fireHydrantDisabled fh || catitemname == "Shield") &&
                                                  (case fireHydrantDir fh of
                                                            DirLeft   -> if catitemname == "Poncho"
                                                                            then case catdirection of
                                                                                     DirLeft   -> False
                                                                                     _         -> rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + rectWidth (fireHydrantRect fh) / 2.0
                                                                            else rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + rectWidth (fireHydrantRect fh) / 2.0
                                                            DirRight  -> if catitemname == "Poncho"
                                                                            then case catdirection of
                                                                                      DirRight  -> False
                                                                                      _         -> rectX catrect + rectWidth catrect < rectX (fireHydrantRect fh) + rectWidth (fireHydrantRect fh)
                                                                            else rectX catrect + rectWidth catrect > rectX (fireHydrantRect fh) + 100))

        catIsWet = catWetFromPuddle || catWetFromRain || catWetFromFireHydrant

        -- reached end marker?
        catWin = rectIntersect catrect (endMarkerRect $ endMarker mainpanel)

        -- update cat item effects
        preEffect = execState (do
                                  e1 <- get
                                  put (if catBouncePogostick
                                          then pogostickEffect2
                                          else e1)

                                  e2 <- get
                                  put (if catFallUmbrella
                                          then fallUmbrellaEffect
                                          else e2)

                                  e3 <- get
                                  put (if catUpsUmbrella
                                          then upsUmbrellaEffect2
                                          else e3)

                                  e4 <- get
                                  put (if catIsWet
                                         then hurtEffect
                                         else e4)

                                  e5 <- get
                                  put (if catWin
                                          then winEffect
                                          else e5)

                                  return ()) noEffect

        (effect, itemL) = foldr (\item (prevEff, prevList) -> if rectIntersect (itemRect item) catrect && (itemName item `notElem` ["Cork", "Tarp"])
                                                                 then (itemEffect item, prevList)
                                                                 else (prevEff, item:prevList))
                                (preEffect, []) (itemList mainpanel)

        -- update cat
        cat' = execState (do
                              -- apply position change
                              c1 <- get
                              put (updateCatPos c1 catPos')

                              -- apply velocity change
                              c2 <- get
                              put (updateCatVel c2 catVel')

                              -- apply direction change
                              c3 <- get
                              put (c3 {catDirection = catdirection'})

                              -- apply item effect
                              c4 <- get
                              put (effect c4)

                              -- update animation
                              c5 <- get
                              put (updateCatAnim c5)

                              -- revert back to walking from spring boots
                              c6 <- get
                              put (if (catItemName c6 == "SpringBoots") && catTouchingSurface && catVelY < 0
                                      then walkEffect c6
                                      else c6)

                              -- revert back to walking from pogostick bounce
                              c7 <- get
                              put (if (catItemName c7 == "Pogostick") && abs catVelY <= 1.0
                                      then walkEffect c7
                                      else c7)

                              -- revert back to walking from falling umbrella
                              c8 <- get
                              put (if (catItemName c8 == "FallUmbrella") && catTouchingSurface
                                      then walkEffect c8
                                      else c8)

                              -- revert back to walking from upsidedown umbrella
                              c9 <- get
                              put (if (catItemName c9 == "UpsUmbrellaActive") && not catTouchingPuddle
                                      then walkEffect c9
                                      else c9)

                              -- update item duration
                              c10 <- get
                              put (updateCatItemDuration c10)

                              -- teleport cat to mouse pos (DEBUG)
                              c11 <- get
                              put (if spaceKeyDown keys
                                      then updateCatPos c11 (mousex - cameraX, mousey - cameraY)
                                      else c11)

                              return ())
                         (cat mainpanel)

        in (cat', itemL)

-- catRectResponse
catRectResponse :: Vector2d -> Vector2d -> Direction -> Nxt.Types.Rect -> Nxt.Types.Rect -> (Vector2d, Direction)
catRectResponse (catX, catY) (catVelX, catVelY) catDir (Rect catRX catRY catRW catRH) (Rect rectx recty rectwidth rectheight) =
    let displaceY = (recty + rectheight) - catY
        displaceDownY = (recty + rectheight) - (catRY + catRH)
        displaceX
          | catVelX < 0.0 = (rectx + rectwidth) - catRX
          | catVelX > 0.0 = rectx - (catRX + catRW)
          | otherwise = 0.0
        oppDir = case catDir of
                    DirLeft  -> DirRight
                    DirRight -> DirLeft

        in execState (do
                         -- vertical displacement
                         ((x1, y1), d1) <- get
                         put (if catVelY > 0.0
                                 then ((x1, y1 - displaceDownY), d1)
                                 else if abs displaceY < abs displaceX
                                         then ((x1, y1 + displaceY), d1)
                                         else ((x1, y1), d1))

                         -- horizontal displacement
                         ((x2, y2), d2) <- get
                         put (if (catRY + catRH < recty + rectheight && catRY >= recty) || (catRY < recty && catVelY <= 0.0)
                                 then ((x2 + displaceX, y2), oppDir)
                                 else ((x2, y2), d2))

                         return ()) ((catX, catY), catDir)

