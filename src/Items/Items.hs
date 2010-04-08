module Items.Items
    (Item(Item),
     itemPos,
     itemTexture,
     itemEffect,
     itemName,
     ItemButton(ItemButton),
     itemButPos,
     itemButTexture,
     itemButDesc,
     itemButItem,
     itemButCount,
     itemRect,
     itemIntersects,
     drawItem,
     drawItemAt,
     drawItemBut,
     mouseOverItemBut) where

import Data.Maybe
import Graphics.Rendering.OpenGL as GL
import Nxt.Graphics
import Nxt.Types
import Input.InputState
import Cat.Cat

data Item = Item
    {
        itemPos         :: Nxt.Types.Vector2d,
        itemTexture     :: Nxt.Types.Texture,
        itemEffect      :: Cat -> Cat,
        itemName        :: String
    }
instance Eq Item where
    i1 == i2    = itemName i1 == itemName i2
    i1 /= i2    = itemName i1 /= itemName i2

data ItemButton = ItemButton
    {
        itemButPos     :: Nxt.Types.Vector2d,
        itemButTexture :: Nxt.Types.Texture,
        itemButDesc    :: String,
        itemButItem    :: Item,
        itemButCount   :: Int
    }

-- itemRect
itemRect :: Item -> Nxt.Types.Rect
itemRect (Item (x,y) t _ _) =
    Nxt.Types.Rect x y (fromIntegral $ textureWidth t) (fromIntegral $ textureHeight t)

-- itemIntersects
itemIntersects :: Item -> Item -> Bool
itemIntersects item1 item2 = rectIntersect (itemRect item1) (itemRect item2)

-- drawItemAt
drawItemAt :: Double -> Double -> Item -> IO ()
drawItemAt x y (Item _ itemTex _ _) = do
    let x' = x - (fromIntegral $ textureWidth itemTex :: Double) / 2.0
        y' = y - (fromIntegral $ textureHeight itemTex :: Double) / 2.0
    Nxt.Graphics.drawTexture x' y' itemTex (1.0::GLdouble)

-- drawItem
drawItem :: Item -> IO ()
drawItem (Item (itemX, itemY) itemTex _ _) =
    Nxt.Graphics.drawTexture itemX itemY itemTex (1.0::GLdouble)

-- drawItemBut
drawItemBut :: ItemButton -> IO ()
drawItemBut (ItemButton (itemPosX, itemPosY) itemTexture _ _ _) =
    Nxt.Graphics.drawTexture itemPosX itemPosY itemTexture (1.0::GLdouble)

-- mouseOverItemBut
mouseOverItemBut :: MousePos -> ItemButton -> Bool
mouseOverItemBut (MousePos mouseX mouseY) (ItemButton (itemX, itemY) itemTexture _ _ _) =
    pointInRect (fromIntegral mouseX, fromIntegral mouseY) itemRect
    where
        itemRect = Nxt.Types.Rect itemX itemY (fromIntegral $ textureWidth itemTexture) (fromIntegral $ textureHeight itemTexture)

