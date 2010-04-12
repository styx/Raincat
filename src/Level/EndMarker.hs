module Level.EndMarker
    (EndMarker(EndMarker),
     endMarkerRect,
     endMarkerTexture,
     initEndMarker) where

import Nxt.Types
import Nxt.Graphics
import Settings.Path

data EndMarker = EndMarker
    {
        endMarkerRect       :: Nxt.Types.Rect,
        endMarkerTexture    :: Nxt.Types.Texture
    }

initEndMarker :: Vector2d -> IO EndMarker
initEndMarker (posX, posY) = do
    dataPath  <- getDataDir
    markerTex <- loadTexture (dataPath ++ "/data/level-misc/level-end-marker.png")

    let markerRect = Nxt.Types.Rect posX posY
                                   (fromIntegral $ textureWidth markerTex :: Double)
                                   (fromIntegral $ textureHeight markerTex :: Double)

    return (EndMarker markerRect markerTex)

