module Level.FireHydrant
    (FireHydrant(FireHydrant),
     fireHydrantDisabled,
     fireHydrantDir,
     fireHydrantRect,
     fireHydrantTexture,
     initFireHydrant,
     updateFireHydrant,
     drawFireHydrant) where

import Graphics.Rendering.OpenGL
import Nxt.Graphics
import Nxt.Types
import Settings.WorldSettings as WorldSettings
import Settings.Path

data FireHydrant = FireHydrant
    {
        fireHydrantDisabled     :: Bool,
        fireHydrantDir          :: Direction,
        fireHydrantRect         :: Nxt.Types.Rect,
        fireHydrantTexture      :: [Nxt.Types.Texture],
        fireHydrantDisTexture   :: Nxt.Types.Texture
    }

-- initFireHydrant
initFireHydrant :: Vector2d -> Direction -> IO FireHydrant
initFireHydrant (posX, posY) dir = do
    dataPath <- getDataDir
    textures <- cycleTextures (dataPath ++ "/data/level-misc/fire-hydrant-left") 8 WorldSettings.fireHydrantFrameTime

    let rect = Nxt.Types.Rect posX posY (fromIntegral $ textureWidth $ head textures) (fromIntegral $ textureHeight $ head textures)

    return (FireHydrant False dir rect textures (head textures))

-- updateFireHydrant
updateFireHydrant :: FireHydrant -> FireHydrant
updateFireHydrant fireHydrant =
    fireHydrant {fireHydrantTexture = tail (fireHydrantTexture fireHydrant)}

-- drawFireHydrant
drawFireHydrant :: FireHydrant -> IO ()
drawFireHydrant (FireHydrant disa dir rect texList texDis) =
    Nxt.Graphics.drawTextureFlip posX posY tex (1.0::GLdouble) flip
    where (posX, posY) = (rectX rect, rectY rect)
          tex = if disa then texDis else head texList
          flip = case dir of
                      DirLeft -> False
                      DirRight -> True

