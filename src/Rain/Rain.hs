module Rain.Rain
    (updateRain,
     drawRain,
     rainRect,
     rainPoly) where

import System.Random
import Graphics.Rendering.OpenGL
import World.World
import Panels.MainPanel
import Nxt.Types
import Items.Items
import Level.Level
import Settings.RainSettings as RainSettings
import Settings.WorldSettings as WorldSettings
import Settings.DisplaySettings as DisplaySettings
import Nxt.Graphics

-- updateRain
updateRain :: WorldState -> IO [Vector2d]
updateRain worldState = do
    let rain = raindrops (mainPanel worldState)
        (cameraX, cameraY) = cameraPos (mainPanel worldState)

    let fallenRain = fallRain rain cameraY

    let spawnList = [(1.0 - cameraX)..(maxWorldX - cameraX)]
    let xPos = [x::Double | x <- spawnList, (ceiling x) `mod` rainSpacing == 0]

    gen <- newStdGen

    let lvlHeight = fromIntegral(levelHeight $ curLevel worldState)::Double
    let yPos = randomRs (lvlHeight - rainHeight - cameraY, lvlHeight - cameraY) (gen)

    let rainPositions = zip xPos yPos

    newRainSeq <- sequence $ map createNewRain rainPositions
    let newRain = concat newRainSeq

    let totalRain = newRain ++ fallenRain
    let rainPolyCol = collideRainPoly totalRain (polySurfaces (mainPanel worldState))
    let rectSurfaces' = map (\tarp -> (itemRect tarp)) (tarpList (mainPanel worldState))
                        ++
                        map (\cork -> (itemRect cork)) (corkList (mainPanel worldState))
                        ++
                        (rectSurfaces (mainPanel worldState))
    let rainRectCol = collideRainRect rainPolyCol rectSurfaces'

    return rainRectCol

-- createNewRain
createNewRain :: Vector2d -> IO ([Vector2d])
createNewRain rainPos = do
    raindropDiceRoll <- getStdRandom $ randomR (0::Int, rainSpawnChance)

    return (if raindropDiceRoll == 0 then [rainPos] else [])

-- fallRain
fallRain :: [Vector2d] -> Double -> [Vector2d]
fallRain [] _ = []
fallRain ((raindropX, raindropY) : rain) cameraY
    | raindropY > (-cameraY) = (raindropX, raindropY - rainFallSpeed) : fallRain rain cameraY
    | otherwise           = fallRain rain cameraY

-- drawRain
drawRain :: [Vector2d] -> IO ()
drawRain [] = return ()
drawRain ((raindropX, raindropY) : rain) = do
    renderPrimitive Quads $ do
        mapM_ (\x -> color x) rainColor
        mapM_ (\x -> vertex x) (raindropVertices raindropX raindropY)
    drawRain rain

-- raindropVertices
raindropVertices :: Double -> Double -> [Vertex3 GLdouble]
raindropVertices x y =
    [Vertex3 x' y' 0.0,
     Vertex3 (x' + rainWidth') y' 0.0,
     Vertex3 (x' + rainWidth') (y' + rainHeight') 0.0,
     Vertex3 x' (y' + rainHeight') 0.0]
     where x' = toGLdouble x
           y' = toGLdouble y
           rainWidth' = toGLdouble rainWidth
           rainHeight' = toGLdouble rainHeight

-- rainPoly
rainPoly :: Vector2d -> Nxt.Types.Poly
rainPoly (raindropX, raindropY) =
    Poly 3 [(raindropX,raindropY),
            (raindropX+RainSettings.rainWidth,raindropY),
            (raindropX+RainSettings.rainWidth,raindropY+RainSettings.rainHeight)]

-- collideRainPoly
collideRainPoly :: [Vector2d] -> [Nxt.Types.Poly] -> [Vector2d]
collideRainPoly [] rect = []
collideRainPoly (raindrop:rain) polys =
    if (foldr (\poly remove -> polyIntersect poly (rainPoly raindrop) || remove) False polys)
       then
       collideRainPoly rain polys
       else
       raindrop:(collideRainPoly rain polys)

-- rainRect
rainRect :: Vector2d -> Nxt.Types.Rect
rainRect (raindropX, raindropY) =
    Rect raindropX raindropY RainSettings.rainWidth RainSettings.rainHeight

-- collideRainRect
collideRainRect :: [Vector2d] -> [Nxt.Types.Rect] -> [Vector2d]
collideRainRect [] rect = []
collideRainRect (raindrop:rain) rects =
    if (foldr (\rect remove -> rectIntersect (rainRect raindrop) rect || remove) False rects)
       then
       collideRainRect rain rects
       else
       raindrop:(collideRainRect rain rects)

