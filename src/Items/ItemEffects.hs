module Items.ItemEffects
    (noEffect,
     hurtEffect,
     winEffect,
     walkEffect,
     springBootsEffect,
     hairDryerEffect,
     speedBootsEffect,
     rainBootsEffect,
     ponchoEffect,
     shieldEffect,
     umbrellaEffect,
     fallUmbrellaEffect,
     upsUmbrellaEffect,
     upsUmbrellaEffect2,
     skateboardEffect,
     pogostickEffect,
     pogostickEffect2,
     wrenchEffect) where

import Data.Maybe
import Graphics.Rendering.OpenGL as GL
import Nxt.Graphics
import Nxt.Types
import Input.InputState
import Cat.Cat
import Settings.CatSettings as CatSettings

-- No Effect
noEffect :: Cat -> Cat
noEffect cat = cat

-- Walk
walkEffect :: Cat -> Cat
walkEffect cat =
    let walkTex = walkTextures $ catAnimations cat
        in cat {catTexture = walkTex, catItemName = "NoItem", catItemDuration = Nothing}

-- Hurt
hurtEffect :: Cat -> Cat
hurtEffect cat =
    let hurtTex = hurtTextures $ catAnimations cat
        in if catItemName cat /= "Hurt"
              then updateCatVel (cat {catTexture = hurtTex, catItemName = "Hurt", catItemDuration = Just 120}) (0.0, 0.0)
              else updateCatVel cat (0.0, 0.0)

-- Win
winEffect :: Cat -> Cat
winEffect cat =
    let winTex = idleTextures $ catAnimations cat
        in if catItemName cat /= "Win"
              then updateCatVel (cat {catTexture = winTex, catItemName = "Win", catItemDuration = Just 120}) (0.0, 0.0)
              else updateCatVel cat (0.0, 0.0)

-- Spring Boots
springBootsEffect :: Cat -> Cat
springBootsEffect cat =
    let springBootsTex = springBootsTextures $ catAnimations cat
        vel = (fst $ catVelocity cat, CatSettings.catSpringVelY)
        in updateCatVel (cat {catTexture = springBootsTex, catItemName = "SpringBoots",
                              catItemDuration = Nothing}) vel

-- Hair Dryer
hairDryerEffect :: Cat -> Cat
hairDryerEffect cat =
    let (velX, velY) = (catVelocity cat)
    in updateCatVel cat (-velX, velY)

-- Speed Boots
speedBootsEffect :: Cat -> Cat
speedBootsEffect cat =
    let speedBootsTex = speedBootsTextures $ catAnimations cat
        vel = (case catDirection cat of
                  DirRight -> CatSettings.catSpeedVelX
                  DirLeft -> (-CatSettings.catSpeedVelX),
               snd $ catVelocity cat)
    in updateCatVel (cat {catTexture = speedBootsTex, catItemName = "SpeedBoots",
                          catItemDuration = Just CatSettings.catSpeedDuration}) vel

-- Rain Boots
rainBootsEffect :: Cat -> Cat
rainBootsEffect cat =
    let rainBootsTex = rainBootsTextures $ catAnimations cat
        in cat {catTexture = rainBootsTex, catItemName = "RainBoots",
                catItemDuration = Just CatSettings.catRainDuration}

-- Poncho
ponchoEffect :: Cat -> Cat
ponchoEffect cat =
    let ponchoTex = ponchoTextures $ catAnimations cat
    in cat {catTexture = ponchoTex, catItemName = "Poncho",
            catItemDuration = Just CatSettings.catPonchoDuration}

-- Shield
shieldEffect :: Cat -> Cat
shieldEffect cat =
    let shieldTex = shieldTextures $ catAnimations cat
    in cat {catTexture = shieldTex, catItemName = "Shield",
            catItemDuration = Just CatSettings.catShieldDuration}

-- Umbrella
umbrellaEffect :: Cat -> Cat
umbrellaEffect cat =
    let umbrellaTex = umbrellaTextures $ catAnimations cat
    in cat {catTexture = umbrellaTex, catItemName = "Umbrella",
            catItemDuration = Just CatSettings.catUmbrellaDuration}

fallUmbrellaEffect :: Cat -> Cat
fallUmbrellaEffect cat =
    let fallUmbrellaTex = fallUmbrellaTextures $ catAnimations cat
        catVel = (fst $ catVelocity cat, CatSettings.catFallUmbrellaVelY)
        itemDur = if isNothing (catItemDuration cat)
                     then Just CatSettings.catUmbrellaDuration
                     else catItemDuration cat
    in cat {catTexture = fallUmbrellaTex, catItemName = "FallUmbrella",
            catItemDuration = itemDur, catVelocity = catVel}

-- Upsidedown Umbrella
upsUmbrellaEffect :: Cat -> Cat
upsUmbrellaEffect cat =
    cat {catItemName = "UpsUmbrella", catItemDuration = Nothing}

upsUmbrellaEffect2 :: Cat -> Cat
upsUmbrellaEffect2 cat =
    let upsUmbrellaTex = upsUmbrellaTextures $ catAnimations cat
        catVel = (fst $ catVelocity cat, 0.0)
        in cat {catTexture = upsUmbrellaTex, catItemName = "UpsUmbrellaActive",
                catItemDuration = Nothing, catVelocity = catVel}

-- Skateboard
skateboardEffect :: Cat -> Cat
skateboardEffect cat =
    let skateboardTex = skateboardTextures $ catAnimations cat
        vel = (case catDirection cat of
                    DirRight -> CatSettings.catSkateVelX
                    DirLeft -> (-CatSettings.catSkateVelX),
               snd (catVelocity cat))
    in updateCatVel (cat {catTexture = skateboardTex, catItemName = "Skateboard",
                          catItemDuration = Just CatSettings.catSkateDuration}) vel

-- Pogostick
pogostickEffect :: Cat -> Cat
pogostickEffect cat =
    cat {catItemName = "Pogostick", catItemDuration = Nothing}

pogostickEffect2 :: Cat -> Cat
pogostickEffect2 cat =
    let pogostickTex = pogostickTextures $ catAnimations cat
    in cat {catTexture = pogostickTex, catItemDuration = Nothing}

-- Wrench
wrenchEffect :: Cat -> Cat
wrenchEffect cat =
    cat {catItemName = "Wrench", catItemDuration = Nothing}

