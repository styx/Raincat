module Panels.MainPanel
    (MainPanel(MainPanel),
     cameraPos,
     raindrops,
     backgroundTexture,
     cat,
     itemList,
     corkList,
     tarpList,
     curItem,
     placingItem,
     musak,
     rectSurfaces,
     polySurfaces,
     puddles,
     fireHydrants,
     endMarker) where

import Graphics.Rendering.OpenGL hiding (Rect)
import Nxt.Types
import Nxt.Audio
import Cat.Cat
import Items.Items
import Level.EndMarker
import Level.FireHydrant

data MainPanel = MainPanel
    {
        cameraPos           :: Vector2d,
        raindrops           :: [Vector2d],
        backgroundTexture   :: Nxt.Types.Texture,
        cat                 :: Cat.Cat.Cat,
        rectSurfaces        :: [Rect],
        polySurfaces        :: [Poly],
        puddles             :: [Rect],
        fireHydrants        :: [FireHydrant],
        endMarker           :: EndMarker,
        itemList            :: [Item],
        corkList            :: [Item],
        tarpList            :: [Item],
        curItem             :: Item,
        placingItem         :: Maybe Item,
        musak               :: Music
    }

