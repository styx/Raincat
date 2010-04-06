module UI.ItemPanel
    (ItemPanel(ItemPanel),
     itemButtonList) where

import Item.Items

data ItemPanel = ItemPanel
    {
        itemButtonlist :: [ItemButton]
    }

