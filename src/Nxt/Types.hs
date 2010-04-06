module Nxt.Types
    (Vector2d,
     Texture(Texture),
     textureWidth,
     textureHeight,
     textureObject,
     Nxt.Types.Rect(Rect),
     rectX,
     rectY,
     rectWidth,
     rectHeight,
     rectRight,
     rectBottom,
     rectIntersect,
     pointInRect,
     overlapRect,
     Poly(Poly),
     polySides,
     polyVertices,
     polyIntersect,
     Direction(DirLeft,DirRight)) where

import Graphics.Rendering.OpenGL as GL hiding (Rect)

-- Direction
data Direction = DirLeft | DirRight

-- Texture
data Texture = Texture
    {
        textureWidth :: GLsizei,
        textureHeight :: GLsizei,
        textureObject :: TextureObject
    }

-- Rect
data Rect = Rect
    {
        rectX :: Double,
        rectY :: Double,
        rectWidth :: Double,
        rectHeight :: Double
    }
    deriving Show

-- rectRight
rectRight :: Rect -> Double
rectRight (Rect x _ w _) = x + w

-- rectBottom
rectBottom :: Rect -> Double
rectBottom (Rect _ y _ h) = y + h

-- rectIntersect
rectIntersect :: Rect -> Rect -> Bool
rectIntersect (Rect r1X r1Y r1Width r1Height) (Rect r2X r2Y r2Width r2Height) =
    not (r1X > (r2X + r2Width) || r2X > (r1X + r1Width) ||
         r1Y > (r2Y + r2Height) || r2Y > (r1Y + r1Height))

-- pointInRect
pointInRect :: Vector2d -> Rect -> Bool
pointInRect (x, y) (Rect rectX rectY rectWidth rectHeight) =
    x >= rectX && x <= (rectX + rectWidth) && y >= rectY && y <= (rectY + rectHeight)

-- overlapRect
overlapRect :: Rect -> Rect -> Rect
overlapRect (Rect r1x r1y r1Width r1Height) (Rect r2x r2y r2Width r2Height) =
    Rect x y width height
    where x = max r1x r2x
          y = max r1y r2y
          width = (min (r1x + r1Width) (r2x + r2Width)) - x
          height = (min (r1y + r1Height) (r2y + r2Height)) - r2Height

-- Vector2d
type Vector2d = (Double, Double)

-- Poly
data Poly = Poly
    {
        polySides       :: Int,
        polyVertices    :: [Vector2d]
    }
    deriving Show

-- dotProduct
dotProduct :: Vector2d -> Vector2d -> Double
dotProduct (p1X, p1Y) (p2X, p2Y) = p1X * p2X + p1Y * p2Y

-- polyIntersect
polyIntersect :: Poly -> Poly -> Bool
polyIntersect polyA polyB =
    let polyAVerts = polyVertices polyA
        polyAPrevVerts = (last polyAVerts):(init polyAVerts)
        polyAPairVerts = zip polyAPrevVerts polyAVerts
        polyBVerts = polyVertices polyB
        polyBPrevVerts = (last polyBVerts):(init polyBVerts)
        polyBPairVerts = zip polyBPrevVerts polyBVerts

        normalizeAxis :: (Vector2d, Vector2d) -> Vector2d
        normalizeAxis (prevVert, vert) =
            let
                (ptPrevX, ptPrevY) = prevVert
                (ptCurrX, ptCurrY) = vert
                axisX = ptPrevY - ptCurrY
                axisY = ptCurrX - ptPrevX
                tmp = sqrt (axisX * axisX + axisY * axisY)
            in
                (axisX / tmp, axisY / tmp)

        projRange :: [Vector2d] -> Vector2d -> (Double,Double)
        projRange vertices axis =
            let
                projLengths = map (\vert -> dotProduct vert axis) vertices
                min = minimum projLengths
                max = maximum projLengths
            in
                (min,max)

        overlap :: (Vector2d, Vector2d) -> Bool
        overlap pairVerts =
            not (maxA < minB || minA > maxB)
            where axis = normalizeAxis pairVerts
                  (minA, maxA) = projRange polyAVerts axis
                  (minB, maxB) = projRange polyBVerts axis

    in
        all overlap polyAPairVerts && all overlap polyBPairVerts

