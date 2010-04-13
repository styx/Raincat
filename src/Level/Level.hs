module Level.Level
    (Level(Level),
     LevelData(LevelData),
     levelData,
     levelWidth,
     levelHeight,
     levelItemCounts,
     levelEnd,
     levelCat,
     levelFireHydrantsL,
     levelFireHydrantsR,
     levelPuddles,
     levelRects,
     levelPolys,
     levelBackgrounds,
     openLevel) where

import Nxt.Types
import System.IO
import qualified Error.Error as E
import Settings.DisplaySettings
import Nxt.Graphics hiding (end)
import Control.Arrow (second)

data LevelData = LevelData
        {
        levelEnd            :: Rect,
        levelCat            :: Rect,
        levelFireHydrantsL  :: [Rect],
        levelFireHydrantsR  :: [Rect],
        levelPuddles        :: [Rect],
        levelRects          :: [Rect],
        levelPolys          :: [Poly],
        levelBackgrounds    :: [(Vector2d, Nxt.Types.Texture)]
        }
data Level = Level
        {
        levelWidth      :: Int,
        levelHeight     :: Int,
        levelItemCounts :: [Int],
        levelData       :: LevelData
        }

-- readInt
readInt' :: String -> Int
readInt' = read

-- readDouble
readDouble' :: String -> Double
readDouble' = read

-- openLevel
openLevel :: String -> IO Level
openLevel file = do
    inh <- openFile file ReadMode
    level <- parseLevel inh
    hClose inh
    return level

-- parseLevel
parseLevel :: Handle -> IO Level
parseLevel inh = do
    levelDimensionS <- hGetLine inh
    let levelDimension = map readInt' (words levelDimensionS)

    itemCountsS <- hGetLine inh
    let itemCountsList = map readInt' (words itemCountsS)
        --itemCounts = initItemCount itemCountsList

    numObjectS <- hGetLine inh
    let numObject = readInt' numObjectS
    let dummyData = LevelData (Rect 0 0 0 0) (Rect 0 0 0 0) [] [] [] [] [] []
    lvlData <- parseShape numObject inh dummyData
    let levelDataT = transformCoord lvlData
    let level = Level (head levelDimension) (last levelDimension) itemCountsList levelDataT

    return level

-- transform coordinates from using top left as (0,0) to bottom left as (0,0)
transformCoord :: LevelData -> LevelData
transformCoord (LevelData end cat fireHydrantsL fireHydrantsR puddles rects polys bgTex) =
    LevelData (transformR end) (transformR cat) (map transformR fireHydrantsL) (map transformR fireHydrantsR) (map transformR puddles) (map transformR rects) (map transformP polys) bgTex
    where transformR (Rect rx ry rw rh) = let sh' = fromGLdouble screenResHeight
                                          in Rect rx (sh' - ry) rw (-rh)
          transformP (Poly polyS polyVs) = let sh' = fromGLdouble screenResHeight
                                           in Poly polyS (map (second (sh' -)) polyVs)

-- parseShape
parseShape :: Int -> Handle -> LevelData -> IO LevelData
parseShape numShapes inh (leveldata@(LevelData _ _ fireHydrantsL fireHydrantsR puddles rects polys _)) = do
    ineof <- hIsEOF inh
    if ineof || numShapes <= 0
      then return leveldata
      else
        do
            coordS <- hGetLine inh
            let toks = words coordS
            let coord = map readDouble' (tail $ words coordS)
            let verts = parseVerts coord
            let poly = Poly (length verts) verts
            let obj = head toks
            let newLevelData = case obj of
                                 "rectangle"        -> leveldata {levelRects = parseRect coord : rects}
                                 "cat"              -> leveldata {levelCat = parseRect coord}
                                 "end"              -> leveldata {levelEnd = parseRect coord}
                                 "firehydrantLeft"  -> leveldata {levelFireHydrantsL = parseRect coord : fireHydrantsL}
                                 "firehydrantRight" -> leveldata {levelFireHydrantsR = parseRect coord : fireHydrantsR}
                                 "puddle"           -> leveldata {levelPuddles = parseRect coord : puddles}
                                 "polygon"          -> leveldata {levelPolys = poly : polys}
                                 _                  -> E.throwEx (E.BadLevelData obj)
            parseShape (numShapes-1) inh newLevelData

-- parseVerts
parseVerts :: [Double] -> [Vector2d]
parseVerts [] = []
parseVerts (_x:[]) = E.throwEx E.BadVerticesData
parseVerts (x:y:vs) = (x,y):parseVerts vs

-- parseRect
parseRect :: [Double] -> Rect
parseRect coords =
    if length coords /= 8
    then E.throwEx E.BadRectData
    else Rect bottomLX bottomLY width height
        where bottomLX = head coords
              bottomLY = coords !! 1
              width = (coords !! 2) - bottomLX
              height = (coords !! 7) - bottomLY

