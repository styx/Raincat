module Cat.Cat
    (idleTextures,
     hurtTextures,
     walkTextures,
     springBootsTextures,
     speedBootsTextures,
     rainBootsTextures,
     ponchoTextures,
     shieldTextures,
     umbrellaTextures,
     skateboardTextures,
     pogostickTextures,
     fallUmbrellaTextures,
     upsUmbrellaTextures,
     laserTextures,
     Cat(Cat),
     initCat,
	 catPos,
     catVelocity,
     catDirection,
	 catTexture,
     catItemName,
     catItemDuration,
     catAnimations,
	 drawCat,
	 catHitbox,
     catPoly,
	 updateCatVel,
     updateCatPos,
     updateCatAnim,
     updateCatItemDuration) where

import Maybe
import Graphics.Rendering.OpenGL as GL
import Nxt.Graphics
import Nxt.Types
import Settings.CatSettings as CatSettings
import Settings.Path

data CatAnimations = CatAnimations
    {
        idleTextures            :: [Nxt.Types.Texture],
        hurtTextures            :: [Nxt.Types.Texture],
        walkTextures            :: [Nxt.Types.Texture],
        springBootsTextures     :: [Nxt.Types.Texture],
        speedBootsTextures      :: [Nxt.Types.Texture],
        rainBootsTextures       :: [Nxt.Types.Texture],
        ponchoTextures          :: [Nxt.Types.Texture],
        shieldTextures          :: [Nxt.Types.Texture],
        umbrellaTextures        :: [Nxt.Types.Texture],
        skateboardTextures      :: [Nxt.Types.Texture],
        pogostickTextures       :: [Nxt.Types.Texture],
        fallUmbrellaTextures    :: [Nxt.Types.Texture],
        upsUmbrellaTextures     :: [Nxt.Types.Texture],
        laserTextures           :: [Nxt.Types.Texture]
    }

data Cat = Cat
	{
		catPos          :: Vector2d,
        catVelocity     :: Vector2d,
        catDirection    :: Direction,
        catTexture      :: [Nxt.Types.Texture],
        catItemName     :: [Char],
        catItemDuration :: Maybe Int,
		catAnimations   :: CatAnimations
	}

-- initCatAnimations
initCatAnimations :: IO (CatAnimations)
initCatAnimations = do
    idleTex         <- cycleTextures (dataPath ++ "data/cat/cat-idle/cat-idle") 1 CatSettings.catWalkFrameTime
    hurtTex         <- cycleTextures (dataPath ++ "data/cat/cat-hurt/cat-hurt") 1 CatSettings.catWalkFrameTime
    walkTex         <- cycleTextures (dataPath ++ "data/cat/cat-walk/cat-walk") 10 CatSettings.catWalkFrameTime
    springBootsTex  <- cycleTextures2 (dataPath ++ "data/cat/cat-springboots/cat-springboots") 3 4 CatSettings.catSpringFrameTime
    speedBootsTex   <- cycleTextures (dataPath ++ "data/cat/cat-speedboots/cat-speedboots") 10 CatSettings.catSpeedFrameTime
    rainBootsTex    <- cycleTextures (dataPath ++ "data/cat/cat-rainboots/cat-rainboots") 10 CatSettings.catRainFrameTime
    ponchoTex       <- cycleTextures (dataPath ++ "data/cat/cat-poncho/cat-poncho") 10 CatSettings.catPonchoFrameTime
    shieldTex       <- cycleTextures (dataPath ++ "data/cat/cat-shield/cat-shield") 10 CatSettings.catShieldFrameTime
    umbrellaTex     <- cycleTextures (dataPath ++ "data/cat/cat-umbrella/cat-umbrella") 10 CatSettings.catUmbrellaFrameTime
    skateboardTex   <- cycleTextures (dataPath ++ "data/cat/cat-skateboard/cat-skateboard") 4 CatSettings.catSkateFrameTime
    pogostickTex    <- cycleTextures2 (dataPath ++ "data/cat/cat-pogostick/cat-pogostick") 2 3 CatSettings.catPogoFrameTime
    fallUmbrellaTex <- cycleTextures (dataPath ++ "data/cat/cat-umbrella/cat-fall-umbrella") 1 CatSettings.catFallUmbrellaFrameTime
    upsUmbrellaTex  <- cycleTextures (dataPath ++ "data/cat/cat-upside-down-umbrella/cat-upside-down-umbrella") 1 CatSettings.catFallUmbrellaFrameTime
    laserTex        <- repeatTexturesN (dataPath ++ "data/cat/cat-laser/cat-laser") 29 30 38 5 41 3

    return (CatAnimations idleTex hurtTex walkTex springBootsTex speedBootsTex rainBootsTex
                          ponchoTex shieldTex umbrellaTex skateboardTex pogostickTex
                          fallUmbrellaTex upsUmbrellaTex laserTex)

-- initCat
initCat :: Vector2d -> IO (Cat)
initCat initPos = do
    animations <- initCatAnimations
    let walkTex = walkTextures animations

    return (Cat initPos (CatSettings.catWalkVelX, 0.0) DirRight walkTex "" Nothing animations)

-- drawCat
drawCat :: Cat -> IO ()
-- the below pattern match is for a very crude hack for the post victory laser screen :(
drawCat (Cat (540.0, 340.0) catVel catDir catTexture _ _ _) =
	Nxt.Graphics.drawTextureFlip 540.0 340.0 (head catTexture) (1.0::GLdouble) False
drawCat (Cat (catPosX, catPosY) catVel catDir catTexture _ _ _) =
	Nxt.Graphics.drawTextureFlip (catPosX - ((fromIntegral $ textureWidth (head catTexture)) / 2)) (catPosY) (head catTexture) (1.0::GLdouble) flip
    where flip = case catDir of
                    DirLeft -> True
                    DirRight -> False

-- catHitbox
catHitbox :: Cat -> Nxt.Types.Rect
catHitbox (Cat (catPosX, catPosY) _ catDir catTexture _ _ _) = 
	Nxt.Types.Rect (catPosX + xOffset - (width / 2)) catPosY  width height
    where width = 50.0
          height = 80.0
          xOffset = case catDir of
                         DirRight -> 25.0
                         DirLeft -> -25.0

-- catPoly
catPoly :: Cat -> Nxt.Types.Poly
catPoly (Cat (catPosX, catPosY) _ catDir catTexture _ _ _) = 
	Poly 4 [(catPosX + xOffset - (width / 2), catPosY),
            (catPosX + xOffset + (width / 2), catPosY),
            (catPosX + xOffset + (width / 2), catPosY + height),
            (catPosX + xOffset - (width / 2), catPosY + height)]
    where width = 50.0
          height = 80.0
          xOffset = case catDir of
                         DirRight -> 25.0
                         DirLeft -> -25.0

-- updateCatVel
updateCatVel :: Cat -> Nxt.Types.Vector2d -> Cat
updateCatVel c@(Cat (catPosX, catPosY) catVel catDir _ _ _ _) (newVelX, newVelY) =
    c {catPos = (catPosX + newVelX, catPosY + newVelY), catVelocity = (newVelX, newVelY),
       catDirection = newDir}
    where newDir = if newVelX < 0.0
                      then DirLeft
                      else if newVelX > 0.0
                              then DirRight
                              else catDir

-- updateCatPos
updateCatPos :: Cat -> Nxt.Types.Vector2d -> Cat
updateCatPos cat pos =
	cat {catPos = pos}

-- updateCatAnim
updateCatAnim :: Cat -> Cat
updateCatAnim cat =
    cat {catTexture = tail (catTexture cat)}

-- updateCatItemDuration
updateCatItemDuration :: Cat -> Cat
updateCatItemDuration c@(Cat _ _ _ _ _ Nothing _) = c
updateCatItemDuration c@(Cat _ _ _ _ _ (Just itemDur) anim) =
    if itemDur <= 0
       then c {catItemDuration = Nothing, catItemName = "NoItem", catTexture = walkTextures anim}
       else c {catItemDuration = Just (itemDur - 1)}
